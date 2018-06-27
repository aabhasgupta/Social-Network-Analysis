setwd("C:/R_Files/ShinyFileUpload")

library(shiny)
library(dplyr)
require(visNetwork)
require(dplyr)
require(networkD3)
require(igraph)
require(DT)
require(shinythemes)



# use the below options code if you wish to increase the file input limit, in this example file input limit is increased from 5MB to 9MB
# options(shiny.maxRequestSize = 9*1024^2)


shinyServer(
  function(input,output,session){
  
  # file$datapath -> gives the path of the file
  ### File 1 as a reactive function
  data <- reactive({
    myfile1 <- input$file1
    if(is.null(myfile1)){return()} 
    read.table(file=myfile1$datapath, sep=input$sep, header = input$header, col.names = c("Sender", "Receiver"))
    
    
  })
  
  
  ### File 2 as a reactive function
  data2 <- reactive({
    myfile2 <- input$file2
    if(is.null(myfile2)){return()} 
    read.table(file=myfile2$datapath, sep=input$sep2, header = input$header2, col.names = c("ID","Dept"))
    
  })
  

  # The number of emails sent and received by each person 
  output$emails_freq <- DT::renderDT({
  
  
  df1 <- read.table(file=input$file1$datapath, sep=input$sep2, header = input$header2, col.names = c("Sender", "Receiver"))
  #df2 <- read.table(file=input$file2$datapath, sep=input$sep2, header = input$header2, col.names = c("Sender", "Receiver"))
  
  temp_sender <- as.data.frame(table(df1$Sender)) 
  temp_receiver <- as.data.frame(table(df1$Receiver))
  temp_both <- merge(temp_sender,temp_receiver, by = "Var1", all = T)
  colnames(temp_both) <- c("User_ID", "Emails_Sent", "Emails_Received")
  if(is.null(df1)){return ()}
  temp_both
  

})
 
  
  # Displaying File1
  output$table <- renderDT({
    if(is.null(data())){return ()}
    datatable(data(), rownames = F, filter = "top")
  })
  
  
  # Displaying the connections 
  output$graph <- renderVisNetwork({
      if(is.null(data())){return ()}
    #visNetwork(edges = head(data(),as.numeric(input$conn)), nodes = data2()$id, fontSize = 16, nodeColour = "blue", zoom = T, opacity = 5, linkColour = "green")  
    
    data <- data()
    network_edges <-  head(data,as.numeric(input$conn))
    colnames(network_edges) <- c("from", "to")
    employeeDept <- data2()
    id <- graph.data.frame(network_edges) %>% get.data.frame(what = "vertices")
    colnames(id) = "id"
    
    return( visNetwork(edges = network_edges, nodes = id, width="100%") %>%  
              visOptions(highlightNearest = list(enabled = TRUE, degree =2, hover= T), nodesIdSelection = T))
  })

  
  # Displaying File2
  output$table2 <- renderDT({
    if(is.null(data2())){return ()}
    datatable(data2(), rownames = F)
  })
  
  
  # Reactive Function for to identify top 10 employees who have sent the maximum e-mails
  Top_10 <- reactive({
    
    data <- data()
    Table_sent <- as.data.frame(sort(table(data$Sender), decreasing = T)) # Sorted by Sender's Frequency
    Top_10 <- head(Table_sent,10)
    colnames(Top_10) <- c("id","freq")
    Top_10
    
  })
  
    
  # Visualising upto two hop neighbours of the Top 10 employees on the basis of Emails Sent 
  output$graph_TopSender <- renderVisNetwork({
    
    if(is.null(data())| is.null(data2())){return ()}
    
    Top_10 <- Top_10()
    data <- data()
    employeeDept <- data2()
    node <- as.numeric(input$node)
    n_1 <- as.numeric(input$oneHop)
    n_2 <- as.numeric(input$twoHop)
    measure <- as.character(input$method)
    
    First_Filter <- data %>% subset(Sender %in% Top_10$id)
    Second_Filter <- data %>% subset(Sender %in% First_Filter$Receiver) # Actually at this step we have got our final table because every person in the top 10 list has sent a mail him/herself
    Final_Table_Top10_Sender <- rbind(First_Filter, Second_Filter)
    Final_Table_Top10_Sender <- as.data.frame(unique(Final_Table_Top10_Sender)) # This step is important

    
        step_1 <- Final_Table_Top10_Sender %>% 
        filter(Final_Table_Top10_Sender$Sender %in% node)  
      
      step_2 <- igraph::graph.data.frame(as.matrix(data), directed = T)
      
      
      
      # step_3:
      if (measure == "Degree") 
      {
        #Network_ID <- get.data.frame(step_2, what = "vertices")
        #colnames(Network_ID) <- "Employee_ID"
        fu <- as.data.frame(degree(step_2, mode = "total"))
        fu$id <- rownames(fu)
        colnames(fu) <- c("degree", "id")
        Network_Degree <- fu %>% subset(id %in% step_1$Receiver)
        Network_Degree <- Network_Degree[!Network_Degree$id == node,]
        Network_Degree <- Network_Degree[order(Network_Degree$degree, decreasing = T),]
        
        step_4 <- head(Network_Degree,n_1)
        
        step_5 <- step_1 %>% subset(Receiver %in% step_4$id)
        
        step_6 <- Final_Table_Top10_Sender %>% subset(Sender %in% step_5$Receiver) 
        
        step_7 <- step_6 %>% subset(!Receiver %in% node )
      
        step_7_a <- head(step_7, n_2)
        
        step_8 <- rbind(step_7_a, step_5)
        
        step_9 <- step_8
        
        colnames(step_9) <- c("from", "to")
        
        step_9$arrows <- "to"
        
      
      id <- employeeDept %>% subset(ID %in% step_9$from | ID %in% step_9$to)
      colnames(id) <- c("id" , "group")
      
      return(visNetwork(edges = step_9, nodes = id, width="100%", height = "700px") %>%  
        visOptions(highlightNearest = list(enabled = TRUE, degree =2, hover= T), nodesIdSelection = T, selectedBy = "group") %>%
        visPhysics(stabilization = T) %>% visInteraction(navigationButtons = T, zoomView = T))
      }
      
      
      if (measure == "Betweeness") 
      {
        
        
        Btw <- as.data.frame(betweenness(step_2))
        Btw$id <- rownames(Btw)
        colnames(Btw) <- c("Betweeness", "id")
        Network_?ness <- Btw %>% subset(id %in% step_1$Receiver)
        Network_Betweeness <- Network_Betweeness[!Network_Betweeness$id == node,]
        Network_Betweeness <- Network_Betweeness[order(Network_Betweeness$Betweeness, decreasing = T),]
        
        step_4 <- head(Network_Betweeness,n_1)
        
        step_5 <- step_1 %>% subset(Receiver %in% step_4$id)
      
        step_6 <- Final_Table_Top10_Sender %>% subset(Sender %in% step_5$Receiver) 
        
        step_7 <- step_6 %>% subset(!Receiver %in% node )
        
        step_7_a <- head(step_7, n_2)
        
        step_8 <- rbind(step_7_a, step_5)
        
        step_9 <- step_8
        
        colnames(step_9) <- c("from", "to")
        
        step_9$arrows <- "to"
        
        id <- employeeDept %>% subset(ID %in% step_9$from | ID %in% step_9$to)
        colnames(id) <- c("id" , "group")
        
        return(visNetwork(edges = step_9, nodes = id, width="100%") %>%  
          visOptions(highlightNearest = list(enabled = TRUE, degree =2, hover= T), nodesIdSelection = T, selectedBy = "group") %>%
          visPhysics(stabilization = T) %>% visInteraction(navigationButtons = T, zoomView = T))
        
        
      }


        

    
  })

  # Reactive Function for to identify top 10 employees who have received the maximum e-mails
  Top_10_Receiver <- reactive({
    
    data <- data()
    Table_received <- as.data.frame(sort(table(data$Receiver), decreasing = T)) # Sorted by Sender's Frequency
    Top_10_Receiver <- head(Table_received,10)
    colnames(Top_10_Receiver) <- c("id","freq")
    Top_10_Receiver
    
  })
  
  # Visualising upto two hop neighbours of the Top 10 employees on the basis of Emails Received 
  output$graph_TopReceiver <- renderVisNetwork({
    
    if(is.null(data())| is.null(data2())){return ()}

    Top_10_Receiver <- Top_10_Receiver()
    data <- data()
    employeeDept <- data2()
    node <- as.numeric(input$node_r)
    n_1 <- as.numeric(input$oneHop_r)
    n_2 <- as.numeric(input$twoHop_r)
    measure <- as.character(input$method_r)
  
    First_Filter <- data %>% subset(Receiver %in% Top_10_Receiver$id)
    Second_Filter <- data %>% subset(Receiver %in% First_Filter$Sender)  
    Final_Table_Top10_Receiver <- rbind(First_Filter, Second_Filter)
    Final_Table_Top10_Receiver <- as.data.frame(unique(Final_Table_Top10_Receiver)) # This step is important
     
    
    step_1 <- Final_Table_Top10_Receiver %>%
      filter(Final_Table_Top10_Receiver$Receiver %in% node)
    
    step_2 <- igraph::graph.data.frame(as.matrix(data), directed = T)
    
    
    
    # step_3:
    if (measure == "Degree")
    {
      fu <- as.data.frame(degree(step_2, mode = "total"))
      fu$id <- rownames(fu)
      colnames(fu) <- c("degree", "id")
      Network_Degree <- fu %>% subset(id %in% step_1$Sender)
      Network_Degree <- Network_Degree[!Network_Degree$id == node,]
      Network_Degree <- Network_Degree[order(Network_Degree$degree, decreasing = T),]
      
      step_4 <- head(Network_Degree,n_1)
      
      step_5 <- step_1 %>% subset(Sender %in% step_4$id)
      
      step_6 <- Final_Table_Top10_Receiver %>% subset(Receiver %in% step_5$Sender)
      
      step_7 <- step_6 %>% subset(!Sender %in% node )
      
      step_7_a <- head(step_7, n_2)
      
      step_8 <- rbind(step_7_a, step_5)
      
      step_9 <- step_8
      
      colnames(step_9) <- c("from", "to")
      
      step_9$arrows <- "to"
      
      
      id <- employeeDept %>% subset(ID %in% step_9$from | ID %in% step_9$to)
      colnames(id) <- c("id" , "group")
      
      return(visNetwork(edges = step_9, nodes = id, width="100%") %>%  
        visOptions(highlightNearest = list(enabled = TRUE, degree =2, hover= T), nodesIdSelection = T, selectedBy = "group") %>%
        visPhysics(stabilization = T) %>% visInteraction(navigationButtons = T, zoomView = T))
    }
    
    
    if (measure == "Betweeness") 
    {
      
      
      Btw <- as.data.frame(betweenness(step_2))
      Btw$id <- rownames(Btw)
      colnames(Btw) <- c("Betweeness", "id")
      Network_Betweeness <- Btw %>% subset(id %in% step_1$Sender)
      Network_Betweeness <- Network_Betweeness[!Network_Betweeness$id == node,]
      Network_Betweeness <- Network_Betweeness[order(Network_Betweeness$Betweeness, decreasing = T),]
      
      step_4 <- head(Network_Betweeness,n_1)
      
      step_5 <- step_1 %>% subset(Sender %in% step_4$id)
      
      step_6 <- Final_Table_Top10_Receiver %>% subset(Receiver %in% step_5$Sender) 
      
      step_7 <- step_6 %>% subset(!Sender %in% node )
      
      step_7_a <- head(step_7, n_2)
      
      step_8 <- rbind(step_7_a, step_5)
      
      step_9 <- step_8
      
      colnames(step_9) <- c("from", "to")
      
      step_9$arrows <- "to"
      
      id <- employeeDept %>% subset(ID %in% step_9$from | ID %in% step_9$to)
      colnames(id) <- c("id" , "group")
      
      return(visNetwork(edges = step_9, nodes = id, width="100%") %>%  
        visOptions(highlightNearest = list(enabled = TRUE, degree =2, hover= T), nodesIdSelection = T, selectedBy = "group") %>%
        visPhysics(stabilization = T) %>% visInteraction(navigationButtons = T, zoomView = T))
      
      
    }
    

    
    
    
  })  
   
  # Reactive function for calculating Degree Centrality in a tabular format for each node
  Degree_Table <- reactive({
    

    data <- data()
     Network_graph <- igraph::graph.data.frame(as.matrix(data), directed = T)
     Network_Nodes <- get.data.frame(Network_graph, what = "vertices")
     Network_Degree<- as.data.frame(degree(Network_graph, mode = "total"))
     temp <- cbind(Network_Nodes, Network_Degree)
     colnames(temp) <- c("id", "degree")
     temp

    
    })
  
  # Displaying the degree centrality table
  output$Degree <- renderDT({
    
    datatable(Degree_Table(), rownames = F)
    
  })
  
  # Reactive Function for to identify top 10 employees who have the highest degree centrality
  Top10_Degree <- reactive({
    
    Degree_Table <- Degree_Table()
    Top10_Degree <- head(arrange(.data = Degree_Table, desc(Degree_Table$degree)),10)
    Top10_Degree 
    
  })
  
  # Visualising upto two hop neighbours of the Top 10 employees on the basis of degree centrality 
  output$graph_Degree <- renderVisNetwork({
    
    if(is.null(data())| is.null(data2())){return ()}
  
    data <- data()
    Top10_Degree <- Top10_Degree()
    fu <- Degree_Table()
    employeeDept <- data2()
    node <- as.numeric(input$node_d)
    n_1 <- as.numeric(input$oneHop_d)
    n_2 <- as.numeric(input$twoHop_d)
    measure <- as.character(input$method_d)
    
    
    Temp1 <- data %>% subset(Sender %in% Top10_Degree$id)
    Temp2 <- data %>% subset(Sender %in% Temp1$Receiver)
    Network_Degree_2hop <- rbind(Temp1, Temp2)
    Network_Degree_2hop <- unique(Network_Degree_2hop)
    colnames(Network_Degree_2hop)  <- c("Sender", "Receiver")
    
  
    step_1 <- Network_Degree_2hop %>% 
      filter(Network_Degree_2hop$Sender %in% node)  
    
    step_2 <- igraph::graph.data.frame(as.matrix(data), directed = T)
    

    # step_3:
    if (measure == "Degree") 
    {
      Network_Degree <- fu %>% subset(id %in% step_1$Receiver)
      Network_Degree <- Network_Degree[!Network_Degree$id == node,]
      Network_Degree <- Network_Degree[order(Network_Degree$degree, decreasing = T),]
      
      step_4 <- head(Network_Degree,n_1)
      
      step_5 <- step_1 %>% subset(Receiver %in% step_4$id)
      
      step_6 <- Network_Degree_2hop %>% subset(Sender %in% step_5$Receiver) 
      
      step_7 <- step_6 %>% subset(!Receiver %in% node )
      
      step_7_a <- head(step_7, n_2)
      
      step_8 <- rbind(step_7_a, step_5)
      
      step_9 <- step_8
      
      colnames(step_9) <- c("from", "to")
      
      step_9$arrows <- "to"
      
      
      id <- employeeDept %>% subset(ID %in% step_9$from | ID %in% step_9$to)
      colnames(id) <- c("id" , "group")
      
     return( visNetwork(edges = step_9, nodes = id, width="100%") %>%  
        visOptions(highlightNearest = list(enabled = TRUE, degree =2, hover= T), nodesIdSelection = T, selectedBy = "group") %>%
        visPhysics(stabilization = T) %>% visInteraction(navigationButtons = T, zoomView = T) )
    }
    
    
    if (measure == "Betweeness") 
    {
      
      
      Btw <- as.data.frame(betweenness(step_2))
      Btw$id <- rownames(Btw)
      colnames(Btw) <- c("Betweeness", "id")
      Network_Betweeness <- Btw %>% subset(id %in% step_1$Receiver)
      Network_Betweeness <- Network_Betweeness[!Network_Betweeness$id == node,]
      Network_Betweeness <- Network_Betweeness[order(Network_Betweeness$Betweeness, decreasing = T),]
      
      step_4 <- head(Network_Betweeness,n_1)
      
      step_5 <- step_1 %>% subset(Receiver %in% step_4$id)
      
      step_6 <- Network_Degree_2hop %>% subset(Sender %in% step_5$Receiver) 
      
      step_7 <- step_6 %>% subset(!Receiver %in% node )
      
      step_7_a <- head(step_7, n_2)
      
      step_8 <- rbind(step_7_a, step_5)
      
      step_9 <- step_8
      
      colnames(step_9) <- c("from", "to")
      
      step_9$arrows <- "to"
      
      
      
      id <- employeeDept %>% subset(ID %in% step_9$from | ID %in% step_9$to)
      colnames(id) <- c("id" , "group")
      
      return(visNetwork(edges = step_9, nodes = id, width="100%") %>%  
        visOptions(highlightNearest = list(enabled = TRUE, degree =2, hover= T), nodesIdSelection = T, selectedBy = "group") %>%
        visPhysics(stabilization = T) %>% visInteraction(navigationButtons = T, zoomView = T))
      
    }
    
    
    if (measure == "Closeness") 
    {
      
      
      Cls <- as.data.frame(closeness(step_2))
      Cls$id <- rownames(Cls)
      colnames(Cls) <- c("Closeness", "id")
      Network_Closeness <- Cls %>% subset(id %in% step_1$Receiver)
      Network_Closeness <- Network_Closeness[!Network_Closeness$id == node,]
      Network_Closeness <- Network_Closeness[order(Network_Closeness$Closeness, decreasing = T),]
      
      step_4 <- head(Network_Closeness,n_1)
      
      step_5 <- step_1 %>% subset(Receiver %in% step_4$id)
      
      step_6 <- Network_Degree_2hop %>% subset(Sender %in% step_5$Receiver) 
      
      step_7 <- step_6 %>% subset(!Receiver %in% node )
      
      step_7_a <- head(step_7, n_2)
      
      step_8 <- rbind(step_7_a, step_5)
      
      step_9 <- step_8
      
      colnames(step_9) <- c("from", "to")
      
      step_9$arrows <- "to"
      
      id <- employeeDept %>% subset(ID %in% step_9$from | ID %in% step_9$to)
      colnames(id) <- c("id" , "group")
      
      return(visNetwork(edges = step_9, nodes = id, width="100%") %>%  
        visOptions(highlightNearest = list(enabled = TRUE, degree =2, hover= T), nodesIdSelection = T, selectedBy = "group") %>%
        visPhysics(stabilization = T) %>% visInteraction(navigationButtons = T, zoomView = T))
      
    }
  
  })
    
  # Reactive function for calculating Betweeness Centrality in a tabular format for each node
  Betweeness_Table <- reactive({
      
      data <- data()
      Network_graph_between <- igraph::graph.data.frame(as.matrix(data), directed = T)
      Network_Nodes_between <- get.data.frame(Network_graph_between, what = "vertices")
      Network_Betweeness<- as.data.frame(betweenness(Network_graph_between))
      temp_between <- cbind(Network_Nodes_between, Network_Betweeness)
      colnames(temp_between) <- c("id", "betweeness")
      temp_between$betweeness <- round(temp_between$betweeness,2)
      temp_between
  })    
    
  # Reactive Function for to identify top 10 employees who have the highest betweeness centrality
  Top10_Betweeness <- reactive({
    
    Betweeness_Table <- Betweeness_Table()
    Top10_Betweeness <- head(arrange(.data = Betweeness_Table, desc(Betweeness_Table$betweeness)),10)
    Top10_Betweeness 
    
  })
  
  # Displaying the betwweness table
  output$Betweeness <- renderDT({
      
      return(datatable(Betweeness_Table(), rownames = F))
      
    })
  
  # Visualising upto two hop neighbours of the Top 10 employees on the basis of betweeness centrality 
  output$graph_Betweeness <- renderVisNetwork({
    
    if(is.null(data())| is.null(data2())){return ()}
    
    data <- data()
    Top10_Betweeness <- Top10_Betweeness()
    fu <- Degree_Table()
    Btw <- Betweeness_Table()
    employeeDept <- data2()
    node <- as.numeric(input$node_b)
    n_1 <- as.numeric(input$oneHop_b)
    n_2 <- as.numeric(input$twoHop_b)
    measure <- as.character(input$method_b)
    
    
    Temp1 <- data %>% subset(Sender %in% Top10_Betweeness$id)
    Temp2 <- data %>% subset(Sender %in% Temp1$Receiver)
    Network_Betweeness_2hop <- rbind(Temp1, Temp2)
    Network_Betweeness_2hop <- unique(Network_Betweeness_2hop)
    colnames(Network_Betweeness_2hop)  <- c("Sender", "Receiver")
    
    
    step_1 <- Network_Betweeness_2hop %>% 
      filter(Network_Betweeness_2hop$Sender %in% node)  
    
    step_2 <- igraph::graph.data.frame(as.matrix(data), directed = T)
    
    if (measure == "Degree") 
    {
      Network_Degree <- fu %>% subset(id %in% step_1$Receiver)
      Network_Degree <- Network_Degree[!Network_Degree$id == node,]
      Network_Degree <- Network_Degree[order(Network_Degree$degree, decreasing = T),]
      
      step_4 <- head(Network_Degree,n_1)
      
      step_5 <- step_1 %>% subset(Receiver %in% step_4$id)
      
      step_6 <- Network_Betweeness_2hop %>% subset(Sender %in% step_5$Receiver) 
      
      step_7 <- step_6 %>% subset(!Receiver %in% node )
      
      step_7_a <- head(step_7, n_2)
      
      step_8 <- rbind(step_7_a, step_5)
      
      step_9 <- step_8
      
      colnames(step_9) <- c("from", "to")
      
      step_9$arrows <- "to"
      
      
      id <- employeeDept %>% subset(ID %in% step_9$from | ID %in% step_9$to)
      colnames(id) <- c("id" , "group")
      
      return( visNetwork(edges = step_9, nodes = id, width="100%") %>%  
                visOptions(highlightNearest = list(enabled = TRUE, degree =2, hover= T), nodesIdSelection = T, selectedBy = "group") %>%
                visPhysics(stabilization = T) %>% visInteraction(navigationButtons = T, zoomView = T) )
    }
    
    if (measure == "Betweeness") 
    {
      
      
      Network_Betweeness <- Btw %>% subset(id %in% step_1$Receiver)
      Network_Betweeness <- Network_Betweeness[!Network_Betweeness$id == node,]
      Network_Betweeness <- Network_Betweeness[order(Network_Betweeness$betweeness, decreasing = T),]
      
      step_4 <- head(Network_Betweeness,n_1)
      
      step_5 <- step_1 %>% subset(Receiver %in% step_4$id)
      
      step_6 <- Network_Betweeness_2hop %>% subset(Sender %in% step_5$Receiver) 
      
      step_7 <- step_6 %>% subset(!Receiver %in% node )
      
      step_7_a <- head(step_7, n_2)
      
      step_8 <- rbind(step_7_a, step_5)
      
      step_9 <- step_8
      
      colnames(step_9) <- c("from", "to")
      
      step_9$arrows <- "to"
      
      
      
      id <- employeeDept %>% subset(ID %in% step_9$from | ID %in% step_9$to)
      colnames(id) <- c("id" , "group")
      
      return(visNetwork(edges = step_9, nodes = id, width="100%") %>%  
               visOptions(highlightNearest = list(enabled = TRUE, degree =2, hover= T), nodesIdSelection = T, selectedBy = "group") %>%
               visPhysics(stabilization = T) %>% visInteraction(navigationButtons = T, zoomView = T))
      
    }
    
    if (measure == "Closeness") 
    {
      
      
      Cls <- as.data.frame(closeness(step_2))
      Cls$id <- rownames(Cls)
      colnames(Cls) <- c("Closeness", "id")
      Network_Closeness <- Cls %>% subset(id %in% step_1$Receiver)
      Network_Closeness <- Network_Closeness[!Network_Closeness$id == node,]
      Network_Closeness <- Network_Closeness[order(Network_Closeness$Closeness, decreasing = T),]
      
      step_4 <- head(Network_Closeness,n_1)
      
      step_5 <- step_1 %>% subset(Receiver %in% step_4$id)
      
      step_6 <- Network_Betweeness_2hop %>% subset(Sender %in% step_5$Receiver) 
      
      step_7 <- step_6 %>% subset(!Receiver %in% node )
      
      step_7_a <- head(step_7, n_2)
      
      step_8 <- rbind(step_7_a, step_5)
      
      step_9 <- step_8
      
      colnames(step_9) <- c("from", "to")
      
      step_9$arrows <- "to"
      
      id <- employeeDept %>% subset(ID %in% step_9$from | ID %in% step_9$to)
      colnames(id) <- c("id" , "group")
      
      return(visNetwork(edges = step_9, nodes = id, width="100%") %>%  
               visOptions(highlightNearest = list(enabled = TRUE, degree =2, hover= T), nodesIdSelection = T, selectedBy = "group") %>%
               visPhysics(stabilization = T) %>% visInteraction(navigationButtons = T, zoomView = T))
      
    }
    
  })
    
  # Aggregating the emails sent for each perso to a department level  
  Email_Dept_table <- reactive({
      
      data <- data()
      employeeDept <- data2()
      
      data_merge1 <- merge(data, employeeDept, by.x = "Sender", by.y = "ID")
      data_merge <- merge(data_merge1,employeeDept, by.x="Receiver", by.y= "ID")
      
      data_merge[,c(1,2)] <- NULL
      colnames(data_merge) <- c("Sender_Dept", "Receiver_Dept")
      table_dept <- as.data.frame(summarise(group_by(data_merge,Sender_Dept,Receiver_Dept),count =n()))
      table_dept
      
    })
  
  # Displaying the aggregated table
  output$Email_Dept <- renderDT({
    if(is.null(data())| is.null(data2())){return ()}
    datatable(Email_Dept_table(), rownames = F)
    
  })
  
  # Visualisisng the aggregated table
  output$graph_Dept <- renderVisNetwork({
    
    if(is.null(data())| is.null(data2())){return ()}
    
    connections <- as.numeric(input$conn2)
    EmployeeDept <- data2()  
    Email_Dept_table <- Email_Dept_table()
    Email_Dept_table <- Email_Dept_table[order(Email_Dept_table$count, decreasing = T),]
    Email_Dept_table <- head(Email_Dept_table, connections)
    colnames(Email_Dept_table) <- c("from", "to", "value")
    Email_Dept_table$arrows <- "to"
    id <- graph.data.frame(Email_Dept_table) %>% get.data.frame(what = "vertices")
    colnames(id) <- "id"
    id <- id %>% subset(id %in% Email_Dept_table$from | id %in% Email_Dept_table$to)
    
    
    

    return( visNetwork(edges = Email_Dept_table, nodes = id, width="100%") %>%  
              visOptions(highlightNearest = list(enabled = TRUE, degree =2, hover= T), nodesIdSelection = T) %>%
              visPhysics(stabilization = T) %>% visInteraction(navigationButtons = T, zoomView = T) )
    
      
  })
  
  # Reactive function for calculating InDegree in a tabular format for each node
  InDegree_Table <- reactive({
    
    
    data <- data()
    Network_graph <- igraph::graph.data.frame(as.matrix(data), directed = T)
    Network_Nodes <- get.data.frame(Network_graph, what = "vertices")
    Network_Degree<- as.data.frame(degree(Network_graph, mode = "in" ))
    temp <- cbind(Network_Nodes, Network_Degree)
    colnames(temp) <- c("id", "indegree")
    temp
    
    
  })
  
  # Displaying the InDegree table
  output$InDegree <- renderDT({
    
    datatable(InDegree_Table(), rownames = F)
    
  })
  
  # Reactive Function for to identify top 10 employees who have the highest betweeness centrality
  Top10_InDegree <- reactive({
    
    InDegree_Table <- InDegree_Table()
    Top10_InDegree <- head(arrange(.data = InDegree_Table, desc(InDegree_Table$indegree)),10)
    Top10_InDegree 
    
  })
  
  # Visualising upto two hop neighbours of the Top 10 employees on the basis of InDegree 
  output$graph_InDegree <- renderVisNetwork({
    
    if(is.null(data())| is.null(data2())){return ()}
    
    data <- data()
    Top10_InDegree <- Top10_InDegree()
    fu <- InDegree_Table()
    employeeDept <- data2()
    node <- as.numeric(input$node_in)
    n_1 <- as.numeric(input$oneHop_in)
    n_2 <- as.numeric(input$twoHop_in)
    measure <- as.character(input$method_in)
    
    
    Temp1 <- data %>% subset(Receiver %in% Top10_InDegree$id)
    Temp2 <- data %>% subset(Receiver %in% Temp1$Sender)
    Network_InDegree_2hop <- rbind(Temp1, Temp2)
    Network_InDegree_2hop <- unique(Network_InDegree_2hop)
    colnames(Network_InDegree_2hop)  <- c("Sender", "Receiver")
    
    
    step_1 <- Network_InDegree_2hop %>% 
      filter(Network_InDegree_2hop$Receiver %in% node)  
    
    step_2 <- igraph::graph.data.frame(as.matrix(data), directed = T)
    
    
    # step_3:
    if (measure == "InDegree") 
    {
      Network_Degree <- fu %>% subset(id %in% step_1$Sender)
      Network_Degree <- Network_Degree[!Network_Degree$id == node,]
      Network_Degree <- Network_Degree[order(Network_Degree$indegree, decreasing = T),]
      
      step_4 <- head(Network_Degree,n_1)
      
      step_5 <- step_1 %>% subset(Sender %in% step_4$id)
      
      step_6 <- Network_InDegree_2hop %>% subset(Receiver %in% step_5$Sender) 
      
      step_7 <- step_6 %>% subset(!Sender %in% node )
      
      step_7_a <- head(step_7, n_2)
      
      step_8 <- rbind(step_7_a, step_5)
      
      step_9 <- step_8
      
      colnames(step_9) <- c("from", "to")
      
      step_9$arrows <- "to"
      
      
      id <- employeeDept %>% subset(ID %in% step_9$from | ID %in% step_9$to)
      colnames(id) <- c("id" , "group")
      
      return( visNetwork(edges = step_9, nodes = id, width="100%") %>%  
                visOptions(highlightNearest = list(enabled = TRUE, degree =2, hover= T), nodesIdSelection = T, selectedBy = "group") %>%
                visPhysics(stabilization = F) %>% visInteraction(navigationButtons = T, zoomView = T) )
    }
    
    
    if (measure == "Betweeness") 
    {
      
      
      Btw <- as.data.frame(betweenness(step_2))
      Btw$id <- rownames(Btw)
      colnames(Btw) <- c("Betweeness", "id")
      Network_Betweeness <- Btw %>% subset(id %in% step_1$Sender)
      Network_Betweeness <- Network_Betweeness[!Network_Betweeness$id == node,]
      Network_Betweeness <- Network_Betweeness[order(Network_Betweeness$Betweeness, decreasing = T),]
      
      step_4 <- head(Network_Betweeness,n_1)
      
      step_5 <- step_1 %>% subset(Sender %in% step_4$id)
      
      step_6 <- Network_InDegree_2hop %>% subset(Receiver %in% step_5$Sender) 
      
      step_7 <- step_6 %>% subset(!Sender %in% node )
      
      step_7_a <- head(step_7, n_2)
      
      step_8 <- rbind(step_7_a, step_5)
      
      step_9 <- step_8
      
      colnames(step_9) <- c("from", "to")
      
      step_9$arrows <- "to"
      
      
      
      id <- employeeDept %>% subset(ID %in% step_9$from | ID %in% step_9$to)
      colnames(id) <- c("id" , "group")
      
      return(visNetwork(edges = step_9, nodes = id, width="100%") %>%  
               visOptions(highlightNearest = list(enabled = TRUE, degree =2, hover= T), nodesIdSelection = T, selectedBy = "group") %>%
               visPhysics(stabilization = T) %>% visInteraction(navigationButtons = T, zoomView = T))
      
    }
    
    
    if (measure == "Closeness") 
    {
      
      
      Cls <- as.data.frame(closeness(step_2))
      Cls$id <- rownames(Cls)
      colnames(Cls) <- c("Closeness", "id")
      Network_Closeness <- Cls %>% subset(id %in% step_1$Sender)
      Network_Closeness <- Network_Closeness[!Network_Closeness$id == node,]
      Network_Closeness <- Network_Closeness[order(Network_Closeness$Closeness, decreasing = T),]
      
      step_4 <- head(Network_Closeness,n_1)
      
      step_5 <- step_1 %>% subset(Sender %in% step_4$id)
      
      step_6 <- Network_InDegree_2hop %>% subset(Receiver %in% step_5$Sender) 
      
      step_7 <- step_6 %>% subset(!Sender %in% node )
      
      step_7_a <- head(step_7, n_2)
      
      step_8 <- rbind(step_7_a, step_5)
      
      step_9 <- step_8
      
      colnames(step_9) <- c("from", "to")
      
      step_9$arrows <- "to"
      
      id <- employeeDept %>% subset(ID %in% step_9$from | ID %in% step_9$to)
      colnames(id) <- c("id" , "group")
      
      return(visNetwork(edges = step_9, nodes = id, width="100%") %>%  
               visOptions(highlightNearest = list(enabled = TRUE, degree =2, hover= T), nodesIdSelection = T, selectedBy = "group") %>%
               visPhysics(stabilization = T) %>% visInteraction(navigationButtons = T, zoomView = T))
      
    }
    
  })
  
 

  # The following renderUI is used to dynamically generate the tabsets when the file is loaded. Until the file is loaded, app will not show the tabset.
  output$tb <- renderUI({     # The input widgets can be dynamically created using renderUI
    if(is.null(data()) & is.null(data2()))
      h4("No File Uploaded")
    else
      tabsetPanel( 
        
                   tabPanel("Data", 
                            tabsetPanel(
                              tabPanel("Email Data", DTOutput("table")),
                              tabPanel("Dept Data", DTOutput("table2")),
                              tabPanel("Links Visualisation", 
                              fluidRow(textInput("conn", label= "Enter the number of connections to be displayed", "10")),         
                              visNetworkOutput("graph")))
                            ),
                   
                   
                   tabPanel( "Frequency",
                            tabsetPanel(
                              tabPanel("Emails Sent/Received", DTOutput("emails_freq")),   
                              tabPanel("Top 10 Senders", 
                              fluidRow( column(3, selectInput("node", "Node to be displayed out of the Top 10", choices = Top_10()$id, selected = Top_10()[1])), 
                                        column(3, textInput("oneHop", "Number of One Hop Neighbors to be Displayed", value = "10" )),
                                        column(3, selectInput("method", "Centrality Measure to filter Neighbors", choices = c("Degree","Betweeness"), selected = "Degree")),
                                        column(3, textInput("twoHop", "Number of Two Hop Neighbors to be Displayed", value = "30" ))),
                              visNetworkOutput("graph_TopSender")),
                              tabPanel("Top 10 Receivers",
                              fluidRow( column(3, selectInput("node_r", "Node to be displayed out of the Top 10", choices = Top_10_Receiver()$id, selected = Top_10_Receiver()[1])), 
                                        column(3, textInput("oneHop_r", "Number of One Hop Neighbors to be Displayed", value = "10" )),
                                        column(3, selectInput("method_r", "Centrality Measure to filter Neighbors", choices = c("Degree","Betweeness"), selected = "Degree")),
                                        column(3, textInput("twoHop_r", "Number of Two Hop Neighbors to be Displayed", value = "30" ))),
                              visNetworkOutput("graph_TopReceiver")))),
                   tabPanel("Degree",
                            tabsetPanel(
                              tabPanel("Table", DTOutput("Degree")),
                              tabPanel("Visualisation",
                              fluidRow(column(3, selectInput("node_d", "Node to be displayed out of the Top 10", choices = Top10_Degree()$id, selected = Top10_Degree()[1])), 
                                       column(3, textInput("oneHop_d", "Number of One Hop Neighbors to be Displayed", value = "10" )),
                                       column(3, selectInput("method_d", "Centrality Measure to filter Neighbors", choices = c("Degree","Betweeness", "Closeness"), selected = "Degree")),
                                       column(3, textInput("twoHop_d", "Number of Two Hop Neighbors to be Displayed", value = "30" ))),
                              visNetworkOutput("graph_Degree")))),
                   tabPanel("Betweeness",
                            tabsetPanel(
                              tabPanel("Table", DTOutput("Betweeness")),
                              tabPanel("Visualisation",
                              fluidRow(column(3, selectInput("node_b", "Node to be displayed out of the Top 10", choices = Top10_Betweeness()$id, selected = Top10_Betweeness()[1])), 
                                       column(3, textInput("oneHop_b", "Number of One Hop Neighbors to be Displayed", value = "10" )),
                                       column(3, selectInput("method_b", "Centrality Measure to filter Neighbors", choices = c("Degree","Betweeness", "Closeness"), selected = "Degree")),
                                       column(3,textInput("twoHop_b", "Number of Two Hop Neighbors to be Displayed", value = "30" ))),
                              visNetworkOutput("graph_Betweeness")))),
                   tabPanel("Email B/w Depts", 
                            tabsetPanel(
                              tabPanel("Table", DTOutput("Email_Dept")),
                              tabPanel("Visualisation", 
                                       fluidRow(h5("Note: Connections displayed are sorted according to the frequency of emails exchanged")),
                                       fluidRow(textInput("conn2", "Number of Connections to  be displayed", value = "10")),
                                       visNetworkOutput("graph_Dept")))),
                   tabPanel("InDegree",
                            tabsetPanel(
                              tabPanel("Table", DTOutput("InDegree")),
                              tabPanel("Visualisation",
                                       fluidRow(column(3, selectInput("node_in", "Node to be displayed out of the Top 10", choices = Top10_InDegree()$id, selected = Top10_Degree()[1])), 
                                                column(3, textInput("oneHop_in", "Number of One Hop Neighbors to be Displayed", value = "10" )),
                                                column(3, selectInput("method_in", "Centrality Measure to filter Neighbors", choices = c("InDegree","Betweeness", "Closeness"), selected = "InDegree")),
                                                column(3,textInput("twoHop_in", "Number of Two Hop Neighbors to be Displayed", value = "30" ))),
                                       visNetworkOutput("graph_InDegree")))),
                   
                   tabPanel("Inferences", h5("1.) Employee with id 160 has the highest degree, in-degree, and beteeness centrality scores which implies", 
                                            br(),
                                            "that this employee is very well connected in the network. He/She influences the flow around the network and",
                                            br(),
                                            "is a good broadcaster, i.e. this person is best placed to influence the entire network most quickly. ",
                                            br(),
                                            "2.) There are six common employees among the top ten employees in all the three categories, not necessarily",
                                            br(),
                                            "holding the same rank in each category.",
                                            br(),
                                            "3.) Employee with id 64, ranks among the top 10 ,in InDegree centrality measures but looses out on the",
                                            br(),
                                            "Top 10 Degree centrality list indicating that the employee has received a lot of mails but might have not",
                                            br(),
                                            "sent mails in the same proportion.",
                                            br(),
                                            "4.) Employee with id 377 bridges nodes in a network but does not send or receive a mail that frequently.",
                                            br(),
                                            "5.) Employee with id 128 receives a lot of mail but does not send mails in the same proportion and does not ",
                                            br(),
                                            "influence the flow around the system."))
                   
                   
                   
              
                    
                   
                   
                   
                   
            
)
      
    
    
})

  })


 

  
   
   
   
   