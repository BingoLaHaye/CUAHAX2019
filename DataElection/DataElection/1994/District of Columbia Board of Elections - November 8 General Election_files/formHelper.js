;var BOE = BOE || {};
(function(ns){
  var $ = jQuery;
  var map = {
    only_numbers: only_numbers
  };

  ns.form = map;


  function only_numbers(event)
  {
     var charCode = (event.which) ? event.which : event.keyCode;
     if (charCode > 31 && (charCode < 48 || charCode > 57))
         return false;
     return true;
  }

})(BOE)


