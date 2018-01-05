-- Work carried out by Luis Fernández Jiménez

package body Client_Collections is

    procedure Free is new 
    
        Ada.Unchecked_Deallocation (Cell, Cell_A);
								
    procedure Add_Client (Collection: in out Collection_Type;
                          EP:         in LLU.End_Point_Type;
                          Nick:       in ASU.Unbounded_String;
                          Unique:     in Boolean) is
                           
        P_Aux:  Cell_A;
	    Finish: Boolean := False;
    
    begin
        
        if Collection.Total = 0 then
-- Si lista vacia, creo primer elemento y lo relleno        
            Collection.P_First := new Cell' (Ep, Nick, Null);
            Collection.Total   := 1;
            
        else
            
            P_Aux := Collection.P_First;
            
            while P_Aux /= Null and not Finish loop
            
                if ASU.To_String(Nick) = ASU.To_String(P_Aux.all.Nick) and 
                   not Unique then
-- Coincide un lector
-- Crear elemento, rellenar elemento y convertir en primer elemento de la lista
                    P_Aux := new Cell' (Ep, Nick, Collection.P_First);
                    Collection.Total := Collection.Total + 1;
                    Collection.P_First := P_Aux;
                    Finish := True;
                
                elsif ASU.To_String(Nick) = ASU.To_String(P_Aux.all.Nick) and 
                      Unique then
-- Coincide un escritor					
				    raise Client_Collection_Error;
				
				else
				
					P_Aux := P_Aux.all.Next;
				
				end if;
            
            end loop;
            
            if P_Aux = Null then
-- Llega al final de la lista sin coincidencias                
                P_Aux := new Cell' (Ep, Nick, Collection.P_First);
                Collection.Total := Collection.Total + 1;
                Collection.P_First := P_Aux;
            
            end if;
            
        end if;
			
    end Add_Client;
    
    procedure Delete_Client (Collection: in out Collection_Type;
                             Nick:       in ASU.Unbounded_String) is
    
        P_Aux:      Cell_A  := Collection.P_First;
        P_Aux_Next: Cell_A  := Collection.P_First;
        Finish:     Boolean := False;
        
    begin
        
        if Collection.Total > 0 then
-- Un puntero apunta un elemento por delante        
            P_Aux_Next := Collection.P_First.all.Next;
               
            while P_Aux /= Null and not Finish loop
            
                if ASU.To_String(Nick) = ASU.To_String(P_Aux.all.Nick) then
-- Si elimino el primero de la coleccion            
                    Free(P_Aux);
                    Collection.P_First := P_Aux_Next;
                    Collection.Total := Collection.Total - 1;
                    Finish := True;
                
                elsif P_Aux.all.Next = Null then
-- Si solo hay un elemento en la coleccion                
                    raise Client_Collection_Error;
                    
                elsif ASU.To_String(Nick) = ASU.To_String(P_Aux_Next.all.Nick) then
-- Si elimino cualquier otro elemento de la coleccion                
                    P_Aux.all.Next := P_Aux_Next.all.Next;
                    Free(P_Aux_Next);
                    Collection.Total := Collection.Total - 1;
                    Finish := True;
                    
                elsif P_Aux.all.Next /= Null then
                     
                    P_Aux := P_Aux.all.Next;
                    P_Aux_Next := P_Aux_Next.all.Next;

                else
--recorre toda la lista y no coincide ninguna palabra
                    raise Client_Collection_Error;
                
                end if;
            
            end loop;
        
        else
-- Si la lista esta vacía        
            raise Client_Collection_Error;
            
        end if;
			    
    end Delete_Client;
    
    function Search_Client (Collection: in Collection_Type;
                            EP:         in LLU.End_Point_Type)
                            return ASU.Unbounded_String is
        
        P_Aux:    Cell_A  := Collection.P_First;
        Finish:   Boolean := False;
        
    begin
    
        if Collection.Total > 0 then
            
            while P_Aux /= Null and not Finish loop
                
                if EP = P_Aux.all.Client_EP then
                    
                    Finish := True;
                    
                elsif P_Aux.all.Next /= Null then
                    
                    P_Aux := P_Aux.all.Next;
                    
                else
                    
                    raise Client_Collection_Error;
                    
                end if;
            
            end loop;
            
            return P_Aux.all.Nick;
        
        else
        
            raise Client_Collection_Error;
        
        end if;
        
    end Search_Client;
    
    procedure Send_To_All (Collection: in Collection_Type;
                           P_Buffer:   access LLU.Buffer_Type) is
                          
        P_Aux: Cell_A := Collection.P_First;
        
    begin
    
        while P_Aux /= Null loop
            
            LLU.Send (P_Aux.all.Client_EP, P_Buffer);    
            P_Aux := P_Aux.all.Next;
                
        end loop;
        
    end Send_To_All;
    
    function Collection_Image (Collection: in Collection_Type)
                               return String is
    
         P_Aux: Cell_A := Collection.P_First;
         
         Count: Integer := 0;
         
         Full_Address: ASU.Unbounded_String;
         IP:           ASU.Unbounded_String;
         Port:         ASU.Unbounded_String;
         List:         ASU.Unbounded_String := ASU.To_Unbounded_String("");
         
    begin
    
        if Collection.Total > 0 then
            
            while P_Aux /= Null loop
-- Full_Address := LOWER_LAYER.INET.UDP.UNI.ADDRESS IP: 193.147.49.72, Port: 1025
                Full_Address := ASU.To_Unbounded_String(LLU.Image(P_Aux.all.Client_EP));
                
                while Count /= 1 loop
                    
                    Count := ASU.Index(Full_Address, Ada.Strings.Maps.To_Set(" :,"));	            
	                Full_Address := ASU.Tail(Full_Address, ASU.Length(Full_Address) - Count);
                
                end loop;
-- Full_Address := 193.147.49.72, Port: 1025                
                Count := ASU.Index(Full_Address, Ada.Strings.Maps.To_Set(" :,"));
                IP := ASU.Head(Full_Address, Count - 1);
-- Full_Address :=  Port: 1025             
                Full_Address := ASU.Tail(Full_Address, ASU.Length(Full_Address) - Count);

                while Count /= 0 loop
                    
                    Count := ASU.Index(Full_Address, Ada.Strings.Maps.To_Set(" :,"));	            
	                Full_Address := ASU.Tail(Full_Address, ASU.Length(Full_Address) - Count);
                    
                end loop;
-- Full_Address := 1025
                Port := ASU.Tail(Full_Address, ASU.Length(Full_Address) - Count);
                
                List := List & ASCII.LF & IP & ":" & Port & " " & P_Aux.all.Nick;
                
                P_Aux := P_Aux.all.Next;
            
            end loop;
            
        else
            
            List := ASU.To_Unbounded_String(ASCII.LF & "List of writer clients is empty");
            
        end if;
        
        return ASU.To_String(List);
        
    end Collection_Image;

end Client_Collections;
