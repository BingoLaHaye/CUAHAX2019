var $ = jQuery;

function handleFaqClick(faqId){
    var $a = $("#FAQLink" + faqId);
    var $icon = $a.find("i");
    var $answer = $("#FAQ"+faqId);
    
    $answer.slideToggle();
    
    if($icon.hasClass("fa-plus-circle")){
        $icon.removeClass("fa-plus-circle");
        $icon.addClass("fa-minus-circle");
    }else{
        $icon.addClass("fa-plus-circle");
        $icon.removeClass("fa-minus-circle");
    }
}

//Accordion icon
$(document).ready(function(){
  $("div[id*=AccordionLayout] > div").first().addClass("accordion-open");
  //no self toggle
  $("div[id*=AccordionLayout] div:nth-child(even)").click(function(){
      $(".accordion-open").removeClass("accordion-open");
      $(this).addClass("accordion-open");
  });

  $("#hierarchical_accordion .accordion-link").click(function(){
      if($(this).hasClass("accordion-open")) {
	$(this).removeClass("accordion-open");
        return;
      }
      $("#hierarchical_accordion .accordion-open").removeClass("accordion-open");
      
      $(this).toggleClass("accordion-open");
  });


});