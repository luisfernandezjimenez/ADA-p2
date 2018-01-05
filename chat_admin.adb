-- Work carried out by Luis Fernández Jiménez

with Lower_Layer_UDP;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Command_Line;
with Chat_Messages;

procedure Chat_Admin is

    package LLU renames Lower_Layer_UDP;
    package ASU renames Ada.Strings.Unbounded;
    package ACL renames Ada.Command_Line;
    package CM  renames Chat_Messages;
    
    use type CM.Message_Type;
--------------------------------------------------------------------------------
-- PROCEDIMIENTOS DEL MENU INTERACTIVO 
--------------------------------------------------------------------------------
    procedure Show_Writers(Buffer:    in out LLU.Buffer_Type;
                           Expired:   in out Boolean;
                           Mess:      in out CM.Message_Type;
                           Admin_EP:  in LLU.End_Point_Type;
                           Server_EP: in LLU.End_Point_Type;
                           Password:  in ASU.Unbounded_String) is
    
        Text: ASU.Unbounded_String;
        
    begin
        
        LLU.Reset(Buffer);
-- Rellenar buffer con mensaje COLLECTION_REQUEST y enviar al servidor
        CM.Message_Type'Output(Buffer'Access, CM.Collection_Request);
        LLU.End_Point_Type'Output(Buffer'Access, Admin_EP);
        ASU.Unbounded_String'Output(Buffer'Access, Password);
        
        LLU.Send(Server_EP, Buffer'Access);
-- Esperar respuesta del servidor        
        LLU.Reset(Buffer);
        LLU.Receive(Admin_EP, Buffer'Access, 5.0, Expired);
-- Si pasan 5 seg sin recibir respuesta termina programa con el siguiente mensaje        
        if Expired then
        
            Ada.Text_IO.Put_Line("Incorrect password");
            LLU.Finalize;
        
        else
            
            Mess := CM.Message_Type'Input(Buffer'Access);
       		Text := ASU.Unbounded_String'Input(Buffer'Access);
       		
       		Ada.Text_IO.Put_Line(ASU.To_String(Text));
            
        end if;
        
    end Show_Writers;
    
    procedure Delete_Writer(Buffer:    in out LLU.Buffer_Type;
                            Server_EP: in LLU.End_Point_Type;
                            Password:  in ASU.Unbounded_String) is
    
        Nick: ASU.Unbounded_String;
    
    begin
        
        Ada.Text_IO.Put("Nick to ban? ");
        Nick := ASU.To_Unbounded_String(Ada.Text_IO.Get_Line);
        
        LLU.Reset(Buffer);
-- Rellenar buffer con mensaje BAN y enviar al servidor
        CM.Message_Type'Output(Buffer'Access, CM.Ban);
        ASU.Unbounded_String'Output(Buffer'Access, Password);
        ASU.Unbounded_String'Output(Buffer'Access, Nick);
        
        LLU.Send(Server_EP, Buffer'Access);
        
    end Delete_Writer;
    
    procedure Shutdown_Server(Buffer:    in out LLU.Buffer_Type;
                              Server_EP: in LLU.End_Point_Type;
                              Password:  in ASU.Unbounded_String) is
    
        Nick: ASU.Unbounded_String;
    
    begin
               
        LLU.Reset(Buffer);
-- Rellenar buffer con mensaje SHUTDOWN y enviar al servidor
        CM.Message_Type'Output(Buffer'Access, CM.Shutdown);
        ASU.Unbounded_String'Output(Buffer'Access, Password);
        
        LLU.Send(Server_EP, Buffer'Access);
               
        Ada.Text_IO.Put_Line(ASCII.LF & "Server shutdown sent");
        
    end Shutdown_Server;
--------------------------------------------------------------------------------
-- MENU INTERACTIVO
--------------------------------------------------------------------------------
	procedure Interactive_Menu(Admin_EP: in LLU.End_Point_Type;
	                           Server_EP: in LLU.End_Point_Type;
                               Password: in ASU.Unbounded_String) is
	    
	    Buffer:    aliased LLU.Buffer_Type(1024);
        Expired:   Boolean := False;
        
        Mess: CM.Message_Type := CM.Init;
	    
	    Option: Natural range 1..4 := 1;

	begin
	
	    while Option /= 4 and not Expired loop
	        
	        begin
                
                Ada.Text_IO.Put_Line(ASCII.LF & "Options: ");
                Ada.Text_IO.Put_Line("1 Show writers collection");
                Ada.Text_IO.Put_Line("2 Ban writer");
                Ada.Text_IO.Put_Line("3 Shutdown server");
                Ada.Text_IO.Put_Line("4 Quit");
                Ada.Text_IO.Put(ASCII.LF & "Your option? ");
                    
                Option := Natural'Value(Ada.Text_IO.Get_Line);
                
                case Option is
                
                    when 1 =>
                    
                        Show_Writers(Buffer, Expired, Mess, Admin_EP, Server_EP, Password);
                    
                    when 2 =>
                    
                        Delete_Writer(Buffer, Server_EP, Password);
                    
                    when 3 =>
                    
                        Shutdown_Server(Buffer, Server_EP, Password);
                    
                    when 4 =>
                    
                        LLU.Finalize;
                   
                end case;
	            
            exception
    
                when Constraint_Error =>

                    Ada.Text_IO.Put_Line(ASCII.LF & "Incorrect Option.");
                    Ada.Text_IO.Put_Line("It must introduce a number " & 
                                         "understood between 1 and 4.");
	
            end;
        
        end loop;
	
	end Interactive_Menu;
--------------------------------------------------------------------------------
-- PROGRAMA PRINCIPAL
--------------------------------------------------------------------------------
    Admin_EP:  LLU.End_Point_Type;
    Server_EP: LLU.End_Point_Type;
    
    Server_name: ASU.Unbounded_String;
    IP_Server:   ASU.Unbounded_String;
    Password:    ASU.Unbounded_String;
    
    Usage_Error: exception;
    
begin

    if (ACL.Argument_Count /= 3)then
        
        raise Usage_Error;
    
    end if;

    Server_Name := ASU.To_Unbounded_String(ACL.Argument(1));
    IP_Server   := ASU.To_Unbounded_String(LLU.To_IP(ASU.To_String(Server_Name)));
    Server_EP   := LLU.Build(ASU.To_String(IP_Server), Integer'Value(ACL.Argument(2)));
    Password    := ASU.To_Unbounded_String(ACL.Argument(ACL.Argument_Count));
    
    LLU.Bind_Any(Admin_EP);
-- Ejecutar Menu Interactivo    
    Interactive_Menu(Admin_EP, Server_EP, Password);
    
exception
    
    when Usage_Error =>

        Ada.Text_IO.Put_Line(ASCII.LF & "Usage: " & ACL.Command_Name & 
                             " + <dir_ip> + <port> + <password>");
        LLU.Finalize;
    
    when Constraint_Error =>

        Ada.Text_IO.Put_Line(ASCII.LF & "The port must be an integer");
        LLU.Finalize;
            
    when Ex:others =>
    
        Ada.Text_IO.Put_Line(ASCII.LF & "Excepción imprevista: " &
                             Ada.Exceptions.Exception_Name(Ex) & " en: " &
                             Ada.Exceptions.Exception_Message(Ex));
        LLU.Finalize;

end Chat_Admin;
