function rcHide(elements, anim = true) {
  if ( Array.isArray(elements) ) {
    for (var element of elements) {
      shinyjs.hide({id: element, anim: anim, animType: 'fade'});
    }
  } else {
    shinyjs.hide({id: elements, anim: anim, animType: 'fade'});
  }
}

function rcShow(elements) {
  if ( Array.isArray(elements) ) {
    for (var element of elements) {
      shinyjs.show({id: element, anim: true, animType: 'fade'});
    }
  } else {
    shinyjs.show({id: elements, anim: true, animType: 'fade'});
  }
}
/*
   try { var plotVar = $(this).attr('id').replace('ddnCumGrowth_', ''); } catch(err) { var plotVar = pltCumGrowthVar; }
        /* $('#' + parentDiv).parents(".dropdown").find('.btn').html($(this).text() + caretUpDownSym); 
        
        if (!$(this).attr('id').includes('chk')) {
          $('#' + parentDiv).find('.btn').html($(this).text() + caretUpDownSym);
          $('#' + parentDiv).find('.ddnSelected').removeClass('ddnSelected');
          $(this).addClass('ddnSelected');
        }
        pltCumGrowthVar = ['Confirmed', 'Deaths', 'Recoveries', 'Active'].includes(plotVar) ? plotVar : pltCumGrowthVar;
        Shiny.setInputValue('pltCumGrowthParams', { plotVar: pltCumGrowthVar, plotLog: $('#chkPltCumGrowthLog').is(':checked') }, { priority: 'event' });
        
        break; */

$(document).on('click','.dropdown-menu li a', function(e){

    var parentDiv = $(this).closest('div').attr('id');
    var caretUpDownSym = '<span class=\"fa fa-stack\"><i class=\"fa fa-caret-down\"></i><i class=\"fa fa-caret-up\"></i></span>';
    var caretSym = '<span class="caret"></span>';
    
    
    var plotParams = { ddnCumGrowth: { idSearchStr: 'ddnCumGrowth_', plotVar: pltCumGrowthVar, plotVarList: ['Confirmed', 'Deaths', 'Recoveries', 'Active'] },
                       ddnNewCases: { idSearchStr: 'ddnNewCases_', plotVar: pltNewCasesVar, plotVarList: ['Confirmed', 'Deaths', 'Recoveries', 'Active'] }
    };
    
    function updateDdn(elem) {
     try { var plotVar = elem.attr('id').replace(plotParams.elem.idSearchStr, ''); } catch(err) { var plotVar = plotParams.elem.plotVar; }
     
     if (elem.attr('id').includes('chk')) {
        $('#' + parentDiv).find('.btn').html(elem.text() + caretUpDownSym);
          $('#' + parentDiv).find('.ddnSelected').removeClass('ddnSelected');
          elem.addClass('ddnSelected');
     }
     
     /* = ['Confirmed', 'Deaths', 'Recoveries', 'Active'].includes(plotVar) ? plotVar : pltCumGrowthVar;
        Shiny.setInputValue('pltCumGrowthParams', { plotVar: pltCumGrowthVar, plotLog: $('#chkPltCumGrowthLog').is(':checked') }, { priority: 'event' });
        */
    }
    
    console.log(parentDiv);
  
    switch(parentDiv) {
      case 'ddnCumGrowth':
        try { var plotVar = $(this).attr('id').replace('ddnCumGrowth_', ''); } catch(err) { var plotVar = pltCumGrowthVar; }
        /* $('#' + parentDiv).parents(".dropdown").find('.btn').html($(this).text() + caretUpDownSym); */
        
        if (!$(this).attr('id').includes('chk')) {
          $('#' + parentDiv).find('.btn').html($(this).text() + caretUpDownSym);
          $('#' + parentDiv).find('.ddnSelected').removeClass('ddnSelected');
          $(this).addClass('ddnSelected');
        }
        
        pltCumGrowthVar = ['Confirmed', 'Deaths', 'Recoveries', 'Active'].includes(plotVar) ? plotVar : pltCumGrowthVar;
        Shiny.setInputValue('pltCumGrowthParams', { plotVar: pltCumGrowthVar, plotLog: $('#chkPltCumGrowthLog').is(':checked') }, { priority: 'event' });
        
        break;
        
      case 'ddnNewCases':
        try { var plotVar = $(this).attr('id').replace('ddnNewCases_', ''); } catch(err) { var plotVar = pltNewCasesVar; }
        
        if (!$(this).attr('id').includes('chk')) {
          $('#' + parentDiv).find('.btn').html($(this).text() + caretUpDownSym);
          $('#' + parentDiv).find('.ddnSelected').removeClass('ddnSelected');
          $(this).addClass('ddnSelected');
        }
        
        pltNewCasesVar = ['dConfirmed', 'dDeaths', 'dRecoveries', 'dActive'].includes(plotVar) ? plotVar : pltNewCasesVar;
        Shiny.setInputValue('pltNewCasesParams', { plotVar: pltNewCasesVar, plotLog: null }, { priority: 'event' });
        
        break;
        
      case 'ddnSortCriteria':
        $('#' + parentDiv).find('.ddnSelected').removeClass('ddnSelected');
        $(this).addClass('ddnSelected');
        
        Shiny.setInputValue('sortDelta', $(this).attr('id') == 'ddnAbsoluteVal' ? false : true, { priority: 'event' });
      
        break;
        
      case 'ddnVarSelect':
        $('#' + parentDiv).find('.btn').html($(this).text() + caretSym);
        $('#' + parentDiv).find('.ddnSelected').removeClass('ddnSelected');
        $(this).addClass('ddnSelected');
        
        Shiny.setInputValue('tblSelect', $(this).attr('id').replace('ddnVarSelect_', ''), { priority: 'event' });
        
        
        break;
        
      default:
      
    }
    
});



$(document).ready(function () {
    navigator.geolocation.getCurrentPosition(onSuccess, onError);
    
    function onError (err) {
      Shiny.onInputChange('geoLocEnabled', false);
    }
    
   function onSuccess (position) {
      setTimeout(function () {
          var coords = position.coords;
          Shiny.onInputChange("geoLocEnabled", true);
          Shiny.onInputChange("geoLocLat", coords.latitude);
          Shiny.onInputChange("geoLocLong", coords.longitude);
      }, 1100) }
  });


function rCovidInit() {
  Shiny.setInputValue('sortDelta', false, {priority: 'event'}); 
  Shiny.setInputValue('tblCountrySelect', '', {priority: 'event'}); 
  Shiny.setInputValue('tblSelect', 'Confirmed', { priority: 'event' });
  /* Remove locSelect */
  Shiny.setInputValue('locSelect', null, {priority: 'event'});
  
  pltCumGrowthVar = 'Confirmed';
  pltNewCasesVar = 'dConfirmed';
  pltCaseOutcomesVar = 'dConfirmed';
  Shiny.setInputValue('pltCumGrowthParams', { plotVar: pltCumGrowthVar, plotLog: false }, { priority: 'event' });
  Shiny.setInputValue('pltNewCasesParams', { plotVar: pltNewCasesVar, plotLog: null}, { priority: 'event' });
  Shiny.setInputValue('pltCaseOutcomesParams', { plotVar: pltCaseOutcomesVar, plotLog: null }, { priority: 'event' });
 
}

$(document).on('shiny:connected', function(event) {
  Shiny.setInputValue('dtlPlotHeight', 320, {priority: 'event'});
  Shiny.setInputValue('dtlPlotWidth', 400, {priority: 'event'});
});

$(document).on('shiny:sessioninitialized', function(event) {
  rCovidInit();
});

function displaySizing(window) {
  if (window.screen.availWidth <= 500) {
    /* Mobile */
    rcHide('pnlSiteLogo', anim = false);
  } else {
    /* Test only remove */
    rcShow('pnlSiteLogo');
  }
}

$(window).resize(function(e) { 
  /*
  document.getElementById('pnlDetailInfo').style.width = 'unset';
  document.getElementById('pnlDetailInfo').style.right = 12;
  if (window.innerHeight <= 900) {
    document.getElementById('pnlDetailInfo').style.bottom = 0;
    document.getElementById('pnlDetailInfo').style.height = 'unset';
  } else {
    document.getElementById('pnlDetailInfo').style.height = document.documentElement.style.getPropertyValue('--dlgDtl-height');
  }
  */
  
  /* Insert code here to check if mobile */
  
  /* $('#pnlDetailPlots').height($(window).screen.availHeight - $('#pnlDetailInfo').position().top - $('#pnlDetailTop').height()); */
  Shiny.setInputValue('dtlPlotWidth', 0.85 * $('#pnlDetailInfo').width(), { priority: 'event' });
  
  /*
  Shiny.setInputValue('dtlPlotHeight', 0.5 * ($("div#pnlDetailInfo").height() - $("div#pnlDetailPlots").position().top),  {priority: 'event'});

  */
  
  
  /*Plotly.relayout('dtlPlot', { width: window.innerWidth / 4, height: window.innerHeight / 4});*/
  displaySizing(window);
});





function displayCountryDetail(Country) {
  rcHide(['pnlGlobalStats', 'pnlToolbar']);
  $('.leaflet-control-layers.leaflet-control-layers-expanded.leaflet-control').hide();
  /* Remove next line */
  
  Shiny.setInputValue('locSelect', Country, {priority: 'event'});
  Shiny.setInputValue('tblCountrySelect', Country, {priority: 'event'});
  rcShow('pnlDetailInfo');
}

function hideCountryDetail() {
  rcHide('pnlDetailInfo');
  rcShow(['pnlGlobalStats', 'pnlToolbar']);
  $('.leaflet-control-layers.leaflet-control-layers-expanded.leaflet-control').show();
}

function openGithubLink() {
  window.open('https://altryx.dev/rcovid2019', '_blank');
}