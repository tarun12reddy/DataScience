console.info("title\tlink\tcompany\tlocation");
for (i = 1; i < 2; i++) {
    //you have to create the search first and then take the url from console and put it here
    theurl = 'https://www.linkedin.com/vsearch/jj?type=jobs&keywords=big%20data&orig=GLHD&rsid=1833761381464271261952&pageKey=JOB%3A%2Fconsumer%2FjobsHome%2Findex&trkInfo=tarId%3A1464271261442&locationType=I&countryCode=us&openFacets=L,C&page_num='+i+'&pt=jobs&rnd=1464271277649';
  $.ajax({url: theurl,
          async: false
         }).done(function(data){
			$.each(data.content.page.voltron_unified_search_json.search.results, 
             function(i, results) {
              console.info( 
                results.job.fmt_jobTitle + "\t" +
                results.job.actions.link_viewJob_2 + "\t" +
                results.job.fmt_companyName + "\t" +
                results.job.fmt_location        
              );
       return true;
      }
    );
  });
  //break;
}


//view console and switch to 'info' filter
//copy results into notpad++ and search for ^$\r\n replace with nothing (in regex mode). (This removes blank lines)

https://www.linkedin.com/vsearch/jj?type=jobs&keywords=big%20data&orig=GLHD&rsid=1833761381464271261952&pageKey=JOB%3A%2Fconsumer%2FjobsHome%2Findex&trkInfo=tarId%3A1464271261442&locationType=I&countryCode=us&openFacets=L,C&page_num=1&pt=jobs&rnd=1464271277649