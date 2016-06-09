console.info("title\tlink\tcompany\tlocation");
theurl = 'https://www.linkedin.com/jobs2/view/123336578?refId=1833761381464273448426&trk=vsrp_jobs_res_pri_act&trkInfo=VSRPsearchId%3A1833761381464273448426%2CVSRPtargetId%3A123336578%2CVSRPcmpt%3Aprimary';
jQuery.ajax({url: theurl,
	  async: false
	 }).done(function(data){
        jQuery.each(data.description, 
          function(description){
          console.info(description);
           return true;
          });
    });

theurl = 'https://www.linkedin.com/jobs2/view/123336578?refId=1833761381464273448426&trk=vsrp_jobs_res_pri_act&trkInfo=VSRPsearchId%3A1833761381464273448426%2CVSRPtargetId%3A123336578%2CVSRPcmpt%3Aprimary';

var response = '';
jQuery.ajax({ type: "GET",   
         url: theurl,   
         async: false,
         success : function(text)
         {
             response = text;
         }
});

html = jQuery.parseHTML(response);