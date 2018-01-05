-- Work carried out by Luis Fernández Jiménez

with Lower_Layer_UDP;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Command_Line;
with Chat_Messages;
with Client_Collections;

procedure Chat_Server is
    
    package LLU renames Lower_Layer_UDP;
    package ASU renames Ada.Strings.Unbounded;
    package ACL renames Ada.Command_Line;
    package CM  renames Chat_Messages;
    package CC  renames Client_Collections;
    
    use type CM.Message_Type;
------------------------------------------------------------------------------- 
-- PROCEDIMIENTOS AUXILIARES   
-------------------------------------------------------------------------------
    procedure Init_Mess(Buffer:  in out LLU.Buffer_Type;
                        Readers: in out CC.Collection_Type;
	                    Writers: in out CC.Collection_Type) is
        
        Client_EP: LLU.End_Point_Type;
        Nick:      ASU.Unbounded_String;
        Text:      ASU.Unbounded_String;
        Unique:    Boolean := False;
        
    begin 

        Client_EP := LLU.End_Point_Type'Input (Buffer'Access);
        Nick      := ASU.Unbounded_String'Input(Buffer'Access);
-- Unique es True si el nick es de un escritor, puesto que no puede haber 2 iguales
        Unique    := ASU.To_String(Nick) /= "reader";

        Ada.Text_IO.Put(ASCII.LF & "INIT received from " & 
                        ASU.To_String(Nick));
-- Añadimos cliente a la lista                
        if ASU.To_String(Nick) = "reader" then
        
            CC.Add_Client(Readers, Client_EP, Nick, Unique);
        
        elsif ASU.To_String(Nick) /= "" then

            CC.Add_Client(Writers, Client_EP, Nick, Unique);
            
            LLU.Reset(Buffer);

            begin
-- Se busca si el end_point ya esta en la coleccion y nos devuelve el nick
                Nick := CC.Search_Client(Writers, Client_EP);

                CM.Message_Type'Output(Buffer'Access, CM.Server);
-- El nick en este caso es server
                ASU.Unbounded_String'Output(Buffer'Access, 
                                            ASU.To_Unbounded_String("server"));
                
                Text := ASU.To_Unbounded_String(ASU.To_String(Nick) & 
                                                " joins to the chat");
	            ASU.Unbounded_String'Output(Buffer'Access, Text);
	            
	            CC.Send_To_All(Readers, Buffer'Access);
	            
            exception
		
		        when CC.Client_Collection_Error =>
-- Si salta excepcion en Search_Client		            
		            Nick := ASU.To_Unbounded_String("Unknown Client");
            
            end;
        
        end if;
        
    exception
		
		when CC.Client_Collection_Error =>
-- Si salta excepcion en Add_Client
			Ada.Text_IO.Put(". IGNORED, nick already used");
    
    end Init_Mess;
    
    procedure Writer_Mess(Buffer:  in out LLU.Buffer_Type;
                          Readers: in CC.Collection_Type;
                          Writers: in CC.Collection_Type) is
        
        Client_EP: LLU.End_Point_Type;
        Nick:      ASU.Unbounded_String;
        Text:      ASU.Unbounded_String;
        
    begin
    
        Client_EP := LLU.End_Point_Type'Input (Buffer'Access);
        Text      := ASU.Unbounded_String'Input(Buffer'Access);
        
        begin
-- Comprobacion de si el EP del cliente esta en la lista
            Nick := CC.Search_Client(Writers, Client_EP);
        
            Ada.Text_IO.Put(ASCII.LF & "WRITER received from " & 
                            ASU.To_String(Nick) & ": " & ASU.To_String(Text));
            
            LLU.Reset(Buffer);
            CM.Message_Type'Output(Buffer'Access, CM.Server);
            ASU.Unbounded_String'Output(Buffer'Access, Nick);
            ASU.Unbounded_String'Output(Buffer'Access, Text);
		            
            CC.Send_To_All(Readers, Buffer'Access);
        
        exception
-- Si salta excepcion en Search_Client		
	        when CC.Client_Collection_Error =>
            
                Ada.Text_IO.Put(ASCII.LF & "WRITER received from " &
                                "unknown client. IGNORED");
            
        end;
    
    end Writer_Mess;
    
    procedure Collection_Request_Mess(Buffer:   in out LLU.Buffer_Type;
                                      Password: in ASU.Unbounded_String;
                                      Writers:  in CC.Collection_Type) is
        
        Admin_EP:       LLU.End_Point_Type;
        Password_Admin: ASU.Unbounded_String;
        List:           ASU.Unbounded_String;
        
    begin
    
        Admin_EP      := LLU.End_Point_Type'Input (Buffer'Access);
        Password_Admin := ASU.Unbounded_String'Input(Buffer'Access);
        
        if ASU.To_String(Password) = ASU.To_String(Password_Admin) then
-- Guardo la lista de clientes escritores en List
            List := ASU.To_Unbounded_String(CC.Collection_Image(Writers));
	        
	        LLU.Reset(Buffer);
	        CM.Message_Type'Output(Buffer'Access, CM.Collection_Data);
	        ASU.Unbounded_String'Output(Buffer'Access, List);
            LLU.Send(Admin_EP, Buffer'Access);
	        
	        Ada.Text_IO.Put(ASCII.LF & "LIST_REQUEST received");
            
        else
        
            Ada.Text_IO.Put(ASCII.LF & "LIST_REQUEST received. " & 
                            "IGNORED, incorrect password");
        
        end if;
    
    end Collection_Request_Mess;
    
    procedure Ban_Mess(Buffer:   in out LLU.Buffer_Type;
                       Password: in ASU.Unbounded_String;
                       Writers:  in out CC.Collection_Type) is
    
        Password_Admin: ASU.Unbounded_String;
        Nick:           ASU.Unbounded_String;
        
    begin
    
        Password_Admin := ASU.Unbounded_String'Input(Buffer'Access);
        Nick           := ASU.Unbounded_String'Input(Buffer'Access);
        
        if ASU.To_String(Password) = ASU.To_String(Password_Admin) then
            
            Ada.Text_IO.Put(ASCII.LF & "BAN received for " & ASU.To_String(Nick));
            CC.Delete_Client(Writers, Nick);
        
        else
        
            Ada.Text_IO.Put(ASCII.LF & "BAN received. " & 
                            "IGNORED, incorrect password");
            
        end if;
        
    exception
		
		when CC.Client_Collection_Error =>

			Ada.Text_IO.Put(". IGNORED, nick not found");
        
    end Ban_Mess;
    
    procedure Shutdown_Mess(Buffer:         in out LLU.Buffer_Type;
                            Password:       in ASU.Unbounded_String;
                            End_of_Program: in out Boolean) is
        
        Password_Admin: ASU.Unbounded_String;
        
    begin
    
        Password_Admin := ASU.Unbounded_String'Input(Buffer'Access);
                    
        if ASU.To_String(Password) = ASU.To_String(Password_Admin) then
           
            End_of_Program := True; -- Salgo del bucle del program principal
            Ada.Text_IO.Put_Line(ASCII.LF & "SHUTDOWN received");
            LLU.Finalize; -- cierro conexion
        
        else
        
            Ada.Text_IO.Put(ASCII.LF & "SHUTDOWN received. " & 
                            "IGNORED, incorrect password");
        end if;
        
    end Shutdown_Mess;
--------------------------------------------------------------------------------
-- PROGRAMA PRINCIPAL
--------------------------------------------------------------------------------    
    Server_EP: LLU.End_Point_Type;
    Buffer:    aliased LLU.Buffer_Type(1024);

    Expired:        Boolean := False;
    End_of_Program: Boolean := False;
    
	Readers_Collection:  CC.Collection_Type;
	Writers_Collection:  CC.Collection_Type;

    Server_name:     ASU.Unbounded_String;
    IP_Server:       ASU.Unbounded_String;
    Password_Server: ASU.Unbounded_String;
    
    Mess: CM.Message_Type := CM.Init;

    Usage_Error: exception;

begin

    if ACL.Argument_Count /= 2 then
        
        raise Usage_Error;
    
    end if;
    
    Server_name := ASU.To_Unbounded_String(LLU.Get_Host_Name);
    IP_Server   := ASU.To_Unbounded_String(LLU.To_IP(ASU.To_String(Server_Name)));
    Server_EP   := LLU.Build(ASU.To_String(IP_Server), Integer'Value(ACL.Argument(1)));
    Password_Server    := ASU.To_Unbounded_String(ACL.Argument(ACL.Argument_Count));
   
    LLU.Bind(Server_EP);

    while not End_of_Program loop

        LLU.Reset(Buffer);

        LLU.Receive(Server_EP, Buffer'Access, 1000.0, Expired);

-- Vemos el tipo de mesaje que nos llega.            
        Mess := CM.Message_Type'Input (Buffer'Access);
        
        case Mess is
        
            when CM.Init =>
                
                Init_Mess(Buffer, Readers_Collection, Writers_Collection);
                
            when CM.Writer =>
            
                Writer_Mess(Buffer, Readers_Collection, Writers_Collection);
                
            when CM.Collection_Request =>
            
                Collection_Request_Mess(Buffer, Password_Server, Writers_Collection);
                
            when CM.Ban =>
            
                Ban_Mess(Buffer, Password_Server, Writers_Collection);
                
            when CM.Shutdown | CM.Server | CM.Collection_data =>
                
                Shutdown_Mess(Buffer, Password_Server, End_of_Program);

        end case;

    end loop;

exception
   
   when Usage_Error =>

        Ada.Text_IO.Put_Line(ASCII.LF & "Usage: " & ACL.Command_Name & 
                             " + <port> + <password>");
        LLU.Finalize;
    
    when Constraint_Error =>

        Ada.Text_IO.Put_Line(ASCII.LF & "The port must be an integer");
        LLU.Finalize;
        
    when Ex:others =>
      
        Ada.Text_IO.Put_Line(ASCII.LF & "Excepción imprevista: " &
                             Ada.Exceptions.Exception_Name(Ex) & " en: " &
                             Ada.Exceptions.Exception_Message(Ex));
        LLU.Finalize;

end Chat_Server;
