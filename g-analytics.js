
  window.dataLayer = window.dataLayer || [];
  function gtag(){dataLayer.push(arguments);}
  gtag('js', new Date());

gtag('config', 'UA-170240821-2');


  
$(document).on('shiny:inputchanged', function(event) {
       if (event.name == 'select.county.rt') {
          gtag('event', event.name, {
			  'event_action': 'select_county_reff',
			  'event_category': 'Nowcasts',
			  'event_label': event.value
		  });
    }
    
      if (event.name == 'select.county.hosp') {
          gtag('event', event.name, {
			  'event_action': 'select_county_hosp_forecasts',
			  'event_category': 'Forecasts',
			  'event_label': event.value
		  });
    }
    
      if (event.name == 'select.county.death') {
          gtag('event', event.name, {
			  'event_action': 'select_county_death_forecasts',
			  'event_category': 'Forecasts',
			  'event_label': event.value
		  });
    }
    
      if (event.name == 'county_ts') {
          gtag('event', event.name, {
			  'event_action': 'select_county_4_scenarios',
			  'event_category': 'Scenarios',
			  'event_label': event.value
		  });
    }
    
          if (event.name == 'selected_crosswalk') {
          gtag('event', event.name, {
			  'event_action': 'select_outcomes',
			  'event_category': 'Scenarios',
			  'event_label': event.value
		  });
    }
    
 });
     


     
$(document).on('click','button#Rt_explain.btn.btn-default.action-button.shiny-bound-input', function() {
          gtag('event', 'Rt.button', {
			  'event_action': 'Button',
			  'event_category': 'Nowcasts',
			  'event_label': 'Rt.button'
       });
       
});

$(document).on('click', 'a#dlRt.btn.btn-default.shiny-download-link.shiny-bound-output', function() {
  gtag('event', 'Rt_state_download', {
			  'event_action': 'Download',
			  'event_category': 'Nowcasts',
			  'event_label': 'Rt_state'
         
       });
});


$(document).on('click', 'a#dlRt.indv.cnty.btn.btn-default.shiny-download-link.shiny-bound-output', function() {
  gtag('event', 'Rt_county_download', {
			  'event_action': 'Download',
			  'event_category': 'Nowcasts',
			  'event_label': 'Rt_county'
         
       });
});

$(document).on('click', 'a#dlRt.cnty.btn.btn-default.shiny-download-link.shiny-bound-output', function() {
  gtag('event', 'Rt_cnty_dot_plot_download', {
			  'event_action': 'Download',
			  'event_category': 'Nowcasts',
			  'event_label': 'Rt_county_dot_plot'
         
       });
});

$(document).on('click', 'a#dlRt.cnty.map.btn.btn-default.shiny-download-link.shiny-bound-output', function() {
  gtag('event', 'Rt_map_download', {
			  'event_action': 'Download',
			  'event_category': 'Nowcasts',
			  'event_label': 'Rt_map'
         
       });
});


$(document).on('click', 'a#dlhosp.btn.btn-default.shiny-download-link.shiny-bound-output', function() {
  gtag('event', 'state_hosp_forecasts', {
			  'event_action': 'Download',
			  'event_category': 'Forecasts',
			  'event_label': 'state_hosp_forecasts'
         
       });
});

$(document).on('click', 'a#dlhosp.cnty.btn.btn-default.shiny-download-link.shiny-bound-output', function() {
  gtag('event', 'county_hosp_forecasts', {
			  'event_action': 'Download',
			  'event_category': 'Forecasts',
			  'event_label': 'county_hosp_forecasts'
         
       });
});

$(document).on('click', 'a#dlDeath.btn.btn-default.shiny-download-link.shiny-bound-output', function() {
  gtag('event', 'state_death_forecasts', {
			  'event_action': 'Download',
			  'event_category': 'Forecasts',
			  'event_label': 'state_death_forecasts'
         
       });
});

$(document).on('click', 'a#dlDeath.cnty.btn.btn-default.shiny-download-link.shiny-bound-output', function() {
  gtag('event', 'county_death_forecasts', {
			  'event_action': 'Download',
			  'event_category': 'Forecasts',
			  'event_label': 'county_death_forecasts'
         
       });
});
