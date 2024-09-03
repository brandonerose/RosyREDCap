
library("Microsoft365R") #install.packages("Microsoft365R")
library("RosyUtils") # remotes::install_github("brandonerose/RosyUtils")

# this will open authentication in Microsoft to allow Microsoft365R to use the graph API.
# outlook <- get_personal_outlook()
outlook <- get_business_outlook() # May have to run several times at first

inbox <- outlook$get_inbox()

# the following function will sample the first 1,000 (n) emails from your inbox
# then it will sort all the from addresses and count how many emails there are
# It will print all of the sampled subject lines from this email to aid your choice
# It will ask if you want to delete anything from this email. 1 for Yes and 2 for No
# then it will search for ALL emails in your inbox from this
# again will show subject but this time for all
# final choice for deleting
# if you choose yes it will print message as it deletes
# you can stop anytime with escape button!
# you have the option to change to full_address = F which will use the root email
# for example searching by from med.miami.edu instead email@med.miami.edu
choose_emails_to_delete_in_bulk(inbox = inbox, full_address = T, use_sender = T, n = 1000)

#or instead will sort my what follows @ sign. all emails from example.com instead of person@example.com
choose_emails_to_delete_in_bulk(inbox = inbox, full_address = F, use_sender = T, n = 1000)

# Or choose to delete just by searching an address! (It will prompt you BEFORE it deletes anything)

choose_emails_to_delete_from(inbox,address = "careeralerts.miami.edu", n= 1000) #replace with real email address

# or maybe you want to delete all senders from annoying.com!
choose_emails_to_delete_from(inbox,address = "annoying.com", n= 1000) #replace with real email address

#-------BELOW IS A DEMO OF THE GENERAL STEPS USED BY THE FUNCTIONS ABOVE -------

#you can list your emails like this!
emails <- inbox$list_emails(n=500) # may take ~10-20 seconds

#RosyUtils can summarize this object for you as a data.frame!
emails_sum <- summarize_emails(emails)

#top 10 emails from your sample of 500
emails_sum$from %>% table() %>% sort(decreasing = T) %>% head(10)

#top 10 emails ROOTS from your sample of 500
emails_sum$from_root %>% table() %>% sort(decreasing = T) %>% head(10)

#top email from
top_email <- (emails_sum$from %>% table() %>% sort(decreasing = T) %>% names())[[1]]

#all the emails from the top_email (max n=1000)
emails_from <- outlook$list_emails(search = paste0("from:",top_email),n=1000)

#final summary of all emails from top_email
emails_from_sum <- summarize_emails(emails_from)

#then we can use Microsoft365R to delete!

# if you don't wish to use the full function above try this!
emails_counted <- count_emails(emails_sum = emails_sum, ADDRESS_TYPE = "sender")
# emails_counted <- count_emails(emails_sum = emails_sum, ADDRESS_TYPE = "sender_root") #alternative search
# emails_counted <- count_emails(emails_sum = emails_sum, ADDRESS_TYPE = "from")  #alternative search
# emails_counted <- count_emails(emails_sum = emails_sum, ADDRESS_TYPE = "from_root")  #alternative search
an_address <- sample(emails_counted$address,1)

choose_emails_to_delete_from(inbox = inbox,address = an_address, n= 1000)
