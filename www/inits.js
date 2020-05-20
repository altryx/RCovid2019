function rCovidInit() {
  Shiny.setInputValue('sortDelta', false, {priority: 'event'}); 
  Shiny.setInputValue('tblCountrySelect', '', {priority: 'event'}); 
  Shiny.setInputValue('locSelect', null, {priority: 'event'});
  Shiny.setInputValue('selectPlot', 'pltGrowth50', {priority: 'event'});
  
}