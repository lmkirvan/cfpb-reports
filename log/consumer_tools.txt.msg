Error in UseMethod("write_html") : no applicable method for 'write_html' applied to an object of class "character" 
Traceback: 
[1] "4: xml2::write_html(x = page, file = paste0(path, \"/\", link_to_name(href_q), "
[2] "       \".html\")) at #11"                                                      
[3] "3: advance_page(start_href, base_url = base_url, dead_q = href_q(restart), "    
[4] "       file_url = file_url) at #60"                                             
[5] "2: main(starter_page, base_url = base_url, file_url = file_url, "               
[6] "       restart = old_log) at #3"                                                
[7] "1: restart(starter_page = \"https://www.consumerfinance.gov/compliance/\", "    
[8] "       base_url = base_url, new_log = \"consumer_tools.txt\", old_log = logs, " 
[9] "       file_url = file_url)"                                                    

