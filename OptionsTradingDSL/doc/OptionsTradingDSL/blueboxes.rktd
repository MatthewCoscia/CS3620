375
((3) 0 () 1 ((q lib "OptionsTradingDSL/main.rkt")) () (h ! (equal) ((c def c (c (? . 0) q graph-decision)) q (2462 . 19)) ((c def c (c (? . 0) q graph-multiple-strategies-3d)) q (1149 . 15)) ((c def c (c (? . 0) q graph-multiple-strategies-2d)) q (1896 . 11)) ((c def c (c (? . 0) q make-strategy)) q (692 . 13)) ((c def c (c (? . 0) q make-strategy/shortcut)) q (0 . 17))))
procedure
(make-strategy/shortcut  name                      
                         #:ticker ticker           
                         #:ticker-price cp         
                        [#:quantity qty            
                         #:volatility vol          
                         #:risk-free-rate rfr]     
                         strategy-type             
                         args ...)             -> strategy?
  name : any/c
  ticker : symbol?
  cp : positive?
  qty : positive-integer? = 1
  vol : (>=/c 0) = 0.2
  rfr : (>=/c 0) = 0.05
  strategy-type : any/c
  args : any/c
procedure
(make-strategy  name                      
                #:ticker ticker           
                #:ticker-price cp         
               [#:volatility vol          
                #:risk-free-rate rfr]     
                legs ...)             -> strategy?
  name : any/c
  ticker : symbol?
  cp : positive?
  vol : (>=/c 0) = 0.2
  rfr : (>=/c 0) = 0.05
  legs : any/c
procedure
(graph-multiple-strategies-3d  strategies                  
                              [#:min-price min-price       
                               #:max-price max-price       
                               #:min-days min-days         
                               #:max-days max-days         
                               #:price-step price-step     
                               #:day-step day-step])   -> void?
  strategies : (listof (list/c strategy? any/c any/c))
  min-price : (or/c #f real?) = #f
  max-price : (or/c #f real?) = #f
  min-days : real? = 1
  max-days : real? = 60
  price-step : real? = 5
  day-step : real? = 2
procedure
(graph-multiple-strategies-2d                                     
                               strategies                         
                              [#:min-price min-price              
                               #:max-price max-price              
                               #:days-since-purchase days-since]) 
 -> void?
  strategies : (listof (list/c strategy? any/c any/c))
  min-price : (or/c #f real?) = #f
  max-price : (or/c #f real?) = #f
  days-since : (or/c #f real?) = #f
procedure
(graph-decision  strategy-triplets                      
                [#:3d use-3d?                           
                 #:min-price min-price                  
                 #:max-price max-price                  
                 #:min-days min-days                    
                 #:max-days max-days                    
                 #:price-step price-step                
                 #:day-step day-step                    
                 #:days-since-purchase days-since]) -> void?
  strategy-triplets : (listof (list/c strategy? any/c any/c))
  use-3d? : boolean? = #f
  min-price : (or/c #f real?) = #f
  max-price : (or/c #f real?) = #f
  min-days : real? = 1
  max-days : (or/c #f real?) = #f
  price-step : real? = 5
  day-step : real? = 2
  days-since : (or/c #f real?) = #f
