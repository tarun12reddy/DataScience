var keyword = ['%22Devops%20Engineer%22'];
var locate = [7];

console.info("title\tlink\tcompany\tlocation\tdescription\tlocate");

for(word in keyword){
  for (loc in locate){
    
	pagecounturl = 'https://www.linkedin.com/vsearch/jj?type=jobs&keywords='+keyword[word]+'&orig=GLHD&rsid=1833761381464271261952&pageKey=JOB%3A%2Fconsumer%2FjobsHome%2Findex&trkInfo=tarId%3A1464271261442&locationType=I&countryCode=us&openFacets=L,C&f_L=us%3A'+locate[loc]+'&page_num=1&pt=jobs&rnd=1464271277649';
	
	console.info(pagecounturl);
	jQuery.ajax({url: theurl,
			  async: false
			 }).done(function(data){
				  var total_results = data.content.page.voltron_unified_search_json.search.baseData.resultCount;
				  var total_pages = total_results/25;

          for (page = 1, maxpages = Math.ceil(total_pages); page < maxpages; page++) { 

              pageurl = 'https://www.linkedin.com/vsearch/jj?type=jobs&keywords='+word+'&orig=GLHD&rsid=1833761381464271261952&pageKey=JOB%3A%2Fconsumer%2FjobsHome%2Findex&trkInfo=tarId%3A1464271261442&locationType=I&countryCode=us&openFacets=L,C&f_L=us%3A'+loc+'&page_num='+page+'&pt=jobs&rnd=1464271277649';

              jQuery.ajax({url: pageurl,
                    async: false
                   }).done(function(data){
                  jQuery.each(data.content.page.voltron_unified_search_json.search.results, 
                     function(page, results) {
                       console.info( 
                      results.job.fmt_jobTitle + "\t" +
                      results.job.actions.link_viewJob_2 + "\t" +
                      results.job.fmt_companyName + "\t" +
                      results.job.fmt_location + "\t" +
                      keyword[word] + "\t" +
                      locate[loc]
                      );
                   return true;
                  });
              });
            }
   });
	
  }
}


//view console and switch to 'info' filter
//copy results into notpad++ and search for ^$\r\n replace with nothing (in regex mode). (This removes blank lines)
