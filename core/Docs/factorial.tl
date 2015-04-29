program 
   var K as int ;
   var FACTORIAL as procedure ;
begin 
   K := readInt ; 
   function FACTORIAL ( K ) 
      if K = 0 then
         writeInt 1 ;
      end ;
      if K > 0 then 
         writeInt K ;
         function FACTORIAL ( K - 1 ) end ;
      end ;
    end ;  
end 