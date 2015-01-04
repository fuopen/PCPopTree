require(BMS)
#source('col_list.r')
##function used from 'BMS' package
#bin2hex, hex2bin
##function used from 'base' package included in R system	

#cols<-rev(col.list)
##convert '0','1' character array to binary form,e.g. c('0','1','0','1')->'0101'	
cb2b<-function(binary.array){
#return(do.call(paste,c(as.list(binary.array),sep='')))
	return(paste(binary.array,collapse=''))
}
##convert binary string to decimal form
b2d<-function(binary.string.form){
	return(strtoi(binary.string.form,base=2))
}
##convert binary string to hexdecimal form
b2h<-function(binary.string.form){
	return(toupper(bin2hex(as.numeric(unlist(strsplit(binary.string.form,split=''))))))
}

##sort leaves
sortleaf<-function(binary.strings,nlayer=NULL){
	leaf.num<-length(binary.strings)
	layer.num=max(sapply(binary.strings,nchar))
	complement.each<-function(bs,maxsize){
		return(ifelse(nchar(bs)<maxsize,b2d(paste(bs,cb2b(rep('0',layer.num-nchar(bs))),sep='')),b2d(bs)))
	}
	leaf.fullids<-sapply(binary.strings,complement.each,maxsize=layer.num)
	tmp<-data.frame(id.dec=leaf.fullids,id.bin=binary.strings,index=1:leaf.num)
	tmp<-tmp[order(tmp$id.dec),]
	tmp$id.bin=as.character(tmp$id.bin)

	if(!is.null(nlayer)){
		color_cluster<-substr(tmp$id.bin,1,nlayer)
		nclust<-length(unique(color_cluster))
		clust.num<-table(color_cluster)
#gap.num<-as.integer(max(min(clust.num)/2,1))
		gap.num<-as.integer(max(min(clust.num)/2,3))
		freq<-clust.num/(sum(clust.num)+gap.num*nclust)
		df<-gap.num/(sum(clust.num)+gap.num*nclust)
		col.pbeg<-c(0,cumsum(freq+df))
		col.pbeg<-col.pbeg[-length(col.pbeg)]
		col.pend<-col.pbeg+freq
		tmp$pos_beg<-rep(col.pbeg,clust.num)
		tmp$pos_end<-rep(col.pend,clust.num)
		tmp$head<-rep(names(clust.num),clust.num)
		map.col.pos<-function(m){
			len<-nrow(m)
			m.min<-m$id.dec[1]
			m.max<-m$id.dec[len]
			m.dis<-ifelse(m.min==m.max,1,m.max-m.min)
			region.len<-m$pos_end[1]-m$pos_beg[1]
#m$pos<-region.len*(m$id.dec-m.min)/m.dis+m$pos_beg[1]
			if(len!=1){
				m$pos<-region.len*(0:(len-1))/(len-1)+m$pos_beg[1]
			}
			else{
				m$pos<-m$pos_beg[1]
			}
			return(m)
		}
		tmp<-do.call(rbind,by(tmp,tmp$head,map.col.pos))
	}	
	return(tmp)
}

###############################################
##class "node" build for trees, in addtion,"node" is S3 method
Node<-function(
	node.id=NULL,	##	'0','1' characters contained in the filename
	node.layer=NULL,##	the node is belong to which layers,e.g. root is belong to layer1
#node.col=NULL,	##	hexidecimal representation,e.g "AABBCCDD"(RGBP)
	number.ids=NULL,##	the individuals contained in this cluster
##node.label=NULL,##	the group name
	lchild=NULL,	##	the left child Node
	rchild=NULL,	##	the right child Node
#parent=NULL		##	the parent Node
	node.pos=NULL,	##	location of the node in the plot
#node.lid=NULL	##	location index of the node in the specific lay
	node.colpos=NULL##	location of the node in the colour space which is a real number in [0,1]
){
	node<-list(
		node.id=node.id,
		node.layer=node.layer,
#	node.col=node.col,
		number.ids=number.ids,
##node.label=node.label,
		lchild = lchild,
		rchild = rchild,
#		rchild = rchild,
#parent = parent
		node.pos=node.pos,
		node.colpos=node.colpos
	)
	class(node)<-"Node"
	invisible(node)
}

generate.col<-function(node.colpos){
	return(rainbow(1,start=node.colpos,end=node.colpos+1e-8))
}

Merge.node<-function(Node1,Node2,if_root=F){
	node.id<-ifelse(if_root,'r',substr(Node1$node.id,1,nchar(Node1$node.id)-1))
	node.layer=Node1$node.layer-1
	number.ids=c(Node1$number.ids,Node2$number.ids)
	nl=length(Node1$number.ids)
	nr=length(Node2$number.ids)
	n =length(number.ids)
	lchild=Node1
	rchild=Node2
	node.pos=(Node1$node.pos+Node2$node.pos)/2
	node.colpos=(nl*Node1$node.colpos+nr*Node2$node.colpos)/n
	MergeNode<-Node(node.id,node.layer,number.ids,lchild,rchild,node.pos,node.colpos)
	return(MergeNode)
}

MakeTreeStack<-function(dir,nlayer=3,nodes.pos=NULL){
	files<-list.files(dir,full.names=T,pattern='ind')
	leaves<-gsub('[^0-9]*(\\d+)\\.ind','\\1',files)
	nodes<-vector(mode='list',length=length(leaves))
	nodes.label<-sortleaf(leaves,nlayer)
	node.com.layer<-max(sapply(nodes.label[[2]],nchar))

	
	if(!is.null(nodes.pos)){
		if(length(nodes.pos)!=length(nodes)){
			nodes.pos=NULL
		}
	}
	tot.len<-0
	for(i in 1:length(nodes)){
		nodes[[i]]<-Node(node.id=nodes.label[i,2],node.layer=node.com.layer)
		nodes.file<-files[nodes.label[i,3]]
		node.inf<-read.table(nodes.file)
		nodes[[i]]$number.ids=as.character(node.inf[[1]])
		names(nodes[[i]]$number.ids)=gsub('^[^.]*\\.(\\w+)\\..*$','\\1',as.character(node.inf[[3]]))
#nodes[[i]]$node.col=cols[[1]][i]
		nodes[[i]]$node.colpos=nodes.label[[7]][i]
		if(!is.null(nodes.pos)){ 
			nodes[[i]]$node.pos=nodes.pos[i]
		}
		else{
			if(i==1){
				nodes[[i]]$node.pos=length(nodes[[i]]$number.ids)/2
			}
			else{
				nodes[[i]]$node.pos=tot.len+length(nodes[[i]]$number.ids)/2
			}
			tot.len<-tot.len+length(nodes[[i]]$number.ids)
		}
	}
	for(i in 1:length(nodes)){
		nodes[[i]]$node.pos=nodes[[i]]$node.pos/tot.len
	}
	return(nodes)
}

ConstructTree<-function(nodes){
	nodes.nextlayer<-list()
	layer.info<-list()
	layer.info[[as.character(nodes[[1]]$node.layer)]]<-list(inds=lapply(nodes,function(m)return(m$number.ids)),cols=sapply(nodes,function(m)generate.col(m$node.colpos)),pos=sapply(nodes,function(m)m$node.pos),pos.pair=lapply(1:length(nodes),function(m)c(NA,NA)),col.pair=lapply(1:length(nodes),function(m)c(NA,NA)))
	while(length(nodes)>2){
		i=1
		node.len<-length(nodes)
		j=1
		layer.cur<-nodes[[1]]$node.layer
		info.pos<-list()
		info.col<-list()
		while(i<=node.len){
			if(i<node.len&&nchar(nodes[[i]]$node.id)==layer.cur&&nchar(nodes[[i+1]]$node.id)==layer.cur&&substr(nodes[[i]]$node.id,1,nchar(nodes[[i]]$node.id)-1)==substr(nodes[[i+1]]$node.id,1,nchar(nodes[[i]]$node.id)-1)){
				nodes.nextlayer[[j]]<-Merge.node(nodes[[i]],nodes[[i+1]])
				info.pos[[j]]<-c(nodes[[i]]$node.pos,nodes[[i+1]]$node.pos)
				info.col[[j]]<-c(generate.col(nodes[[i]]$node.colpos),generate.col(nodes[[i+1]]$node.colpos))
#info.col[[j]]<-c(cols[[11-layer.cur]][i],cols[[11-layer.cur]][i+1])
				i=i+2
				j=j+1
			}
			else{
				nodes.nextlayer[[j]]<-nodes[[i]]
				if(nchar(nodes.nextlayer[[j]]$node.id)==layer.cur){
					nodes.nextlayer[[j]]$node.id<-substr(nodes[[i]]$node.id,1,nchar(nodes[[i]]$node.id)-1)
				}
				nodes.nextlayer[[j]]$node.layer<-layer.cur-1
				info.pos[[j]]<-c(NA,NA)
				info.col[[j]]<-c(NA,NA)
				i=i+1
				j=j+1
			}
		}
		nodes<-nodes.nextlayer
		nodes.nextlayer<-list()
		layer.info[[as.character(nodes[[1]]$node.layer)]]<-list(inds=lapply(nodes,function(m)return(m$number.ids)),cols=sapply(nodes,function(m)generate.col(m$node.colpos)),pos=sapply(nodes,function(m)m$node.pos),pos.pair=info.pos,col.pair=info.col)
	}
	node.root<-Merge.node(nodes[[1]],nodes[[2]],T)
	layer.info[['r']]<-list(inds=list(node.root$number.ids),cols=generate.col(node.root$node.colpos),pos=node.root$node.pos,pos.pair=list(c(nodes[[1]]$node.pos,nodes[[2]]$node.pos)),col.pair=list(c(generate.col(nodes[[1]]$node.colpos),generate.col(nodes[[2]]$node.colpos))))
	return(layer.info)
}

Ergodic_same<-function(dir,figure.name,nlayer=3){
	nodestacks<-MakeTreeStack(dir,nlayer)
	inds.info<-do.call(rbind,lapply(nodestacks,function(m)cbind(names(m$number.ids),m$node.pos,m$node.colpos)))
	ind.table<-ConstructTree(nodestacks)
	rec.num<-length(ind.table)
	total.inds<-length(ind.table[[rec.num]]$inds[[1]])
	rec.unit<-1/total.inds
	leaf.names<-lapply(nodestacks,function(m)unique(names(m$number.ids)))
	leaf.names<-sapply(leaf.names,function(m)paste0(m,collapse='/'))
	pdf(figure.name)
	par(plt=c(0,1,0,1),lwd=2)
	plot(0,0,xlab=NA,ylab=NA,type='n',main=NA,axes=F,xlim=c(0,1),ylim=c(-2.2,4),asp=5/35)
	y.bottom=0
	pos.cen=c()
	for(i in 1:rec.num){
		r.xl=0
		r.xr=0
		y.top=y.bottom+0.3
		p<-c()
		for(j in 1:length(ind.table[[i]]$cols)){
			base.col<-substr(ind.table[[i]]$cols[j],1,7)
			r.len=length(ind.table[[i]]$inds[[j]])/total.inds
			r.xr=r.xl+r.len
			if(i==rec.num){
				pos.cen=c(pos.cen,mean(r.xl,r.xr))
			}
			rect(r.xl,y.bottom,r.xr,y.top,col=paste0(base.col,'80'),border=paste0(base.col,'40'),lwd=0.4)
			y.pos<-(y.bottom+y.top)/2
			if(i!=rec.num){
				lines(rep(ind.table[[i]]$pos[j],2),c(y.pos,y.top),col=base.col)
			}
			if(all(!is.na(ind.table[[i]]$pos.pair[[j]]))){
				lines(rep(ind.table[[i]]$pos.pair[[j]][1],2),c(y.pos,y.bottom),col=ind.table[[i]]$col.pair[[j]][1])
				lines(rep(ind.table[[i]]$pos.pair[[j]][2],2),c(y.pos,y.bottom),col=ind.table[[i]]$col.pair[[j]][2])
				lines(c(ind.table[[i]]$pos[j],ind.table[[i]]$pos.pair[[j]][1]),c(y.pos,y.pos),col=ind.table[[i]]$col.pair[[j]][1])
				lines(c(ind.table[[i]]$pos[j],ind.table[[i]]$pos.pair[[j]][2]),c(y.pos,y.pos),col=ind.table[[i]]$col.pair[[j]][2])
			}
			else{
				lines(rep(ind.table[[i]]$pos[j],2),c(y.pos,y.bottom),col=base.col)
			}
			if(i==1){
				text(ind.table[[i]]$pos[j],y.bottom-0.02,leaf.names[[j]],col=base.col,srt=-45,cex=0.4,adj=c(0,1))
			}
			r.xl=r.xr
			p<-c(p,r.len)
		}
		y.bottom=y.top
	}
	inds.info.pop<-inds.info[,1]
	pop.labels<-unique(inds.info.pop)
	inds.info.pos<-as.numeric(inds.info[,2])
	inds.info.colpos<-as.numeric(inds.info[,3])
	inds.info<-data.frame(pop=inds.info.pop,pos=inds.info.pos,col.pos=inds.info.colpos)
	
	pop.pos<-tapply(inds.info$pos,inds.info$pop,mean)
	pop.colpos<-tapply(inds.info$col.pos,inds.info$pop,mean)
	pop.col<-sapply(pop.colpos,generate.col)
	n.inds<-length(inds.info.pop)
	for(i in 1:n.inds){
		x.l<-(i-1)*rec.unit
		x.r<-i*rec.unit
		y.b<--0.51
		y.t<--0.39
		rect(x.l,y.b,x.r,y.t,col=pop.col[[inds.info.pop[i]]],border=pop.col[[inds.info.pop[i]]])
	}
	x.tl<-0
	for(j in 1:length(pop.labels)){
		x.pos<-pop.pos[[pop.labels[j]]]
		text(x.pos,-0.55,pop.labels[j],col=pop.col[[pop.labels[j]]],cex=0.9,srt=-25,adj=c(0,1))
	}
	dev.off()
}

Ergodic_oppo<-function(dir,figure.name,nlayer=3){
	nodestacks<-MakeTreeStack(dir,nlayer)
	inds.info<-do.call(rbind,lapply(nodestacks,function(m)cbind(names(m$number.ids),m$node.pos,m$node.colpos)))
	ind.table<-ConstructTree(nodestacks)
	rec.num<-length(ind.table)
	total.inds<-length(ind.table[[rec.num]]$inds[[1]])
	rec.unit<-1/total.inds
	leaf.names<-lapply(nodestacks,function(m)unique(names(m$number.ids)))
	leaf.names<-sapply(leaf.names,function(m)paste0(m,collapse='/'))
	pdf(figure.name)
	par(plt=c(0,1,0,1),lwd=2)
	plot(0,0,xlab=NA,ylab=NA,type='n',main=NA,axes=F,xlim=c(0,1),ylim=c(-5.2,4))
	y.bottom=0
	y.mbottom=-1.1
	pos.cen=c()
	for(i in 1:rec.num){
		r.xl=0
		r.xr=0
		y.top=y.bottom+0.27
		y.mtop=y.mbottom-0.27
		p<-c()
		for(j in 1:length(ind.table[[i]]$cols)){
			base.col<-substr(ind.table[[i]]$cols[j],1,7)
			r.len=length(ind.table[[i]]$inds[[j]])/total.inds
			r.xr=r.xl+r.len
			if(i==rec.num){
				pos.cen=c(pos.cen,mean(r.xl,r.xr))
			}
			rect(r.xl,y.bottom,r.xr,y.top,col=base.col,border=base.col,lwd=1.1)
			y.pos<-(y.mbottom+y.mtop)/2
			if(i!=rec.num){
				lines(rep(ind.table[[i]]$pos[j],2),c(y.pos,y.mtop),col=base.col)
			}
			if(all(!is.na(ind.table[[i]]$pos.pair[[j]]))){
				lines(rep(ind.table[[i]]$pos.pair[[j]][1],2),c(y.pos,y.mbottom),col=ind.table[[i]]$col.pair[[j]][1])
				lines(rep(ind.table[[i]]$pos.pair[[j]][2],2),c(y.pos,y.mbottom),col=ind.table[[i]]$col.pair[[j]][2])
				lines(c(ind.table[[i]]$pos[j],ind.table[[i]]$pos.pair[[j]][1]),c(y.pos,y.pos),col=ind.table[[i]]$col.pair[[j]][1])
				lines(c(ind.table[[i]]$pos[j],ind.table[[i]]$pos.pair[[j]][2]),c(y.pos,y.pos),col=ind.table[[i]]$col.pair[[j]][2])
			}
			else{
				lines(rep(ind.table[[i]]$pos[j],2),c(y.pos,y.mbottom),col=base.col)
			}
			if(i==1){
				text(ind.table[[i]]$pos[j],y.mbottom+0.03,leaf.names[[j]],col=base.col,srt=45,cex=0.4,adj=c(0,0))
			}
			r.xl=r.xr
			p<-c(p,r.len)
		}
		y.bottom=y.top+0.03
		y.mbottom=y.mtop
	}
	inds.info.pop<-inds.info[,1]
	pop.labels<-unique(inds.info.pop)
	inds.info.pos<-as.numeric(inds.info[,2])
	inds.info.colpos<-as.numeric(inds.info[,3])
	inds.info<-data.frame(pop=inds.info.pop,pos=inds.info.pos,col.pos=inds.info.colpos)
	
	pop.pos<-tapply(inds.info$pos,inds.info$pop,mean)
	pop.colpos<-tapply(inds.info$col.pos,inds.info$pop,mean)
	pop.col<-sapply(pop.colpos,generate.col)
	n.inds<-length(inds.info.pop)
	for(i in 1:n.inds){
		x.l<-(i-1)*rec.unit
		x.r<-i*rec.unit
		y.b<--0.53
		y.t<--0.42
		rect(x.l,y.b,x.r,y.t,col=pop.col[[inds.info.pop[i]]],border=pop.col[[inds.info.pop[i]]])
	}
	x.tl<-0
	for(j in 1:length(pop.labels)){
		x.pos<-pop.pos[[pop.labels[j]]]
		text(x.pos,-0.33,pop.labels[j],col=pop.col[[pop.labels[j]]],cex=0.7,srt=15,adj=c(0,0))
	}
	dev.off()
}

Ergodic<-function(dir,figure.name,nlayer=3,which='same'){
	if(which=='opposite'){
		Ergodic_oppo(dir,figure.name,nlayer)
	}
	else{
		Ergodic_same(dir,figure.name,nlayer)
	}
}

args<-commondArgs(T)
if(length(args)==2){
	Ergodic(args[1],args[2])
}
else{
	if(length(args)==3){
		Ergodic(args[1],args[2],as.numeric(args[3]))
	}
	else{
		Ergodic(args[1],args[2],as.numeric(args[3]),args[4])
	}
}
q(save='no')
