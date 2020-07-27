var initVoice = function() {
if (annyang) {

Shiny.onInputChange("dimension", "category");
Shiny.onInputChange("metric", "sales");
Shiny.onInputChange("metric1", "sales");
Shiny.onInputChange("metric2", "profit");
Shiny.onInputChange("metric3", "sales");
Shiny.onInputChange("metric4", "sales");
Shiny.onInputChange("metric5", "quantity");
Shiny.onInputChange("dimension1", "month");
Shiny.onInputChange("dimension2", "year");
Shiny.onInputChange("dimension3", "category");
Shiny.onInputChange("dimension4", "mode");
Shiny.onInputChange("dimension5", "segment");
Shiny.onInputChange("h", 30);



  
    var commands = {

"aggregate :metric by :dimension": function(metric, dimension) {

      Shiny.onInputChange("metric", metric);
      Shiny.onInputChange("dimension", dimension);
      
      agg = "aggregating ";
      by = " by ";
      
      responsiveVoice.speak(agg.concat(metric,by,dimension), "UK English Male");

    },
    
"plot :metric1 versus :metric2 by :dimension5": function(metric1,metric2, dimension5) {
      
      Shiny.onInputChange("metric1", metric1);
      Shiny.onInputChange("metric2", metric2);
      Shiny.onInputChange("dimension5", dimension5);

      
      plt = "plotting ";
      vs = " versus ";
      by = " by ";

      
      responsiveVoice.speak(plt.concat(metric1,vs,metric2,by,dimension5), "UK English Male");

    },
    
"size by :metric5": function(metric5) {
    
      Shiny.onInputChange("metric5", metric5);
      

      
      siz = "sizing ";
      by = " by ";

      
      responsiveVoice.speak(siz.concat(by,metric5), "UK English Male");

    },
    

"show :metric3 forecast for the next :h days": function(metric3,h) {
      
      Shiny.onInputChange("metric3", metric3);
      Shiny.onInputChange("h", h);

      
      showing = "showing ";
      met = metric3;
      for_cast = " forecast";
      nxt = " for the next ";
      number_of_days = h;
      days = " days";

      
      responsiveVoice.speak(showing.concat(met,for_cast,nxt,number_of_days,days), "UK English Male");

    },    

"show :metric4 heatmap for :dimension1 and :dimension2": function(metric4,dimension1,dimension2) {
      
      Shiny.onInputChange("metric4", metric4);
      Shiny.onInputChange("dimension1", dimension1);
      Shiny.onInputChange("dimension2", dimension2);

      
      showing = "showing ";
      metr = metric4;
      hmap = " heatmap";
      fr = " for ";
      dim1 = dimension1;
      andd = " and ";
      dim2 = dimension2;

      
      responsiveVoice.speak(showing.concat(metr,hmap,fr,dim1,andd,dim2), "UK English Male");

    },
    



"show sankey chart for :dimension3 and :dimension4": function(dimension3,dimension4) {
      
      Shiny.onInputChange("dimension3", dimension3);
      Shiny.onInputChange("dimension4", dimension4);
      
      showing = "showing ";
      skey = "sankey chart for";
      dim3 = dimension3;
      andd = " and ";
      dim4 = dimension4;
      
      
      responsiveVoice.speak(showing.concat(skey,dim3,andd,dim4), "UK English Male");

    }


  };
  


annyang.addCommands(commands);

  
  
annyang.start();
  }
};

$(function() {
  setTimeout(initVoice, 500);
});


