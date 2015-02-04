nodes <- c("B","A","BB","C")
edges <- list( A= list( C= 0.7*0.2*75000*100,BB=1000000), B= list( C= 0.1*0.3*75000*100 ),BB= list( C= 0.3*0.3*75000*100 ))
r <- makeRiver( nodes, edges, node_xpos= c( 2,2,1,3 ),node_styles= list( A= list( col= "yellow" )) )
plot( r )

na=100000
plot(makeRiver(c("A","B","C","D","E"),list(A=list(B=0.7*na,C=0.3*na),B=list(D=0.8*0.7*na,E=0.2*0.7*na),C=list(D=0.9*0.3*na,E=0.1*0.3*na)),
               node_xpos=c(1,2,2,3,3),
               node_labels= c( A= "Auction churners (2013-2014)", B= "Don't contact save desk", C= "Contact save desk",D="Auction Churners",E= "Retained with HC rules"),
               default_style= list( col="#FF8080" ),
               node_styles= list( E= list( col= "red" ))),lty=1)
