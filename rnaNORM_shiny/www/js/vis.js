
$(document).ready(function() {
  
  // create a click handler which listens for a click on the element with id equal to RStudio
  $("#submit").on("click", function(){
  
    Shiny.addCustomMessageHandler("mymessage", function(message) {
        var container = new fornac.FornaContainer("#rna1", {'applyForce': true, 'allowPanningAndZooming': true});
    
        var options = {'structure': message.str1,
                       'sequence':  message.sequence};
        
        var options2 = {'structure': message.str2,
                       'sequence':  message.sequence};
    
        container.addRNA(options.structure, options);
        container.addRNA(options2.structure, options2);

        container.setSize();
        
        //var container2 = new fornac.FornaContainer("#rna2", {'applyForce': true, 'allowPanningAndZooming': false});
        //container2.addRNA(options2.structure, options);
        //container2.setSize();
       
        seq = new Sequence(message.sequence);
        seq.render('#sequence-viewer',{ 'showLineNumbers': true, 'toolbar': true, 'charsPerLine': 100,  'badge': false, 'header':{'display': false } });
        
        seq = new Sequence(message.str1);
        seq.render('#str1-viewer',{ 'showLineNumbers': true, 'toolbar': true, 'charsPerLine': 100,   'badge': false ,   'title' : "", 'header':{'display': false }});
        
        seq = new Sequence(message.str2);
        seq.render('#str2-viewer',{ 'showLineNumbers': true, 'toolbar': true,   'badge': false , 'charsPerLine':100,  'title' : "", 'header':{'display': false }});
        
    });
});
});