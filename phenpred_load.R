function () 
{
	options(stringsAsFactors=FALSE)

	data=read.csv("mapping.txt",sep="\t",header=T)
	labels=data[,2]
	names(labels)=data[,1]

	data=read.csv("otu.txt",sep="\t",check.names=F,header=T)
	#data=data[1:100,]
	for(x in 2:(dim(data)[2]-1)){
		cs=sum(data[,x])
		data[,x]=100.0*data[,x]/cs
	}

	last_col=dim(data)[2]

	level1=c();	level2=c();
	level3=c();	level4=c();
	level5=c();	level6=c();
	level7=c();

	taxon_map=data.frame()
	for(x in 1:dim(data)[1]){
		cur_entry=(data[x,last_col])
		cur_entry=strsplit(cur_entry,",")
		level1=c(level1,cur_entry[[1]][1]);level2=c(level2,cur_entry[[1]][2]);level3=c(level3,cur_entry[[1]][3]);level4=c(level4,cur_entry[[1]][4]);level5=c(level5,cur_entry[[1]][5]);level6=c(level6,cur_entry[[1]][6]);level7=c(level7,cur_entry[[1]][7]);
		taxon_map=rbind(taxon_map,c(cur_entry[[1]][1],cur_entry[[1]][2],cur_entry[[1]][3],cur_entry[[1]][4],cur_entry[[1]][5],cur_entry[[1]][6],cur_entry[[1]][7]))
	}
	level1_u=unique(level1);level2_u=unique(level2);level3_u=unique(level3);level4_u=unique(level4);level5_u=unique(level5);level6_u=unique(level6);level7_u=unique(level7)
	MAP=data.frame()
	for(x in 1:length(level7_u)){
		idx=which(taxon_map[,7]==level7_u[x])
		cs=apply(data[idx,2:(last_col-1)],2,sum)
		MAP=rbind(MAP,cs)
	}
	
	colnames(MAP)=labels[colnames(data)[2:(dim(data)[2]-1)]]
	rownames(MAP)=level7_u
	cn=colnames(MAP)
	MAP=MAP[,grep("20_drug_LS|20_drug_NL",colnames(MAP))]

	df=as.data.frame(t(MAP))
	xx=rownames(df)
	xx[grep("LS",xx)]="LS"
	xx[grep("NL",xx)]="NL"
	df=cbind(df,xx)
	write.table(df,"xxx.txt",sep=",",row.names=F)

}
