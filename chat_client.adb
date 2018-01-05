-- Work carried out by Luis Fernández Jiménez

with Lower_Layer_UDP;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Command_Line;
with Chat_Messages;

procedure Chat_Client is
    
    package LLU renames Lower_Layer_UDP;
    package ASU renames Ada.Strings.Unbounded;
    package ACL renames Ada.Command_Line;
    package CM  renames Chat_Messages;
    
    use type CM.Message_Type;
   
    Server_EP: LLU.End_Point_Type;
    Client_EP: LLU.End_Point_Type;
    Buffer:    aliased LLU.Buffer_Type(1024);
    Expired:   Boolean;

    Server_name: ASU.Unbounded_String;
    IP_Server:   ASU.Unbounded_String;
    Nick:        ASU.Unbounded_String;
    Text:        ASU.Unbounded_String;
    
    Mess: CM.Message_Type := CM.Init;
    
    Usage_Error: exception;
    
begin
    
    if ACL.Argument_Count /= 3 then
        
        raise Usage_Error;
    
    end if;
    
-- Construye el End_Point en el que está atado el servidor
    Server_Name := ASU.To_Unbounded_String(ACL.Argument(1));
    IP_Server   := ASU.To_Unbounded_String(LLU.To_IP(ASU.To_String(Server_Name)));
    Server_EP   := LLU.Build(ASU.To_String(IP_Server), Integer'Value(ACL.Argument(2)));
    Nick        := ASU.To_Unbounded_String(ACL.Argument(ACL.Argument_Count));

-- Construye un End_Point libre cualquiera y se ata a él
    LLU.Bind_Any(Client_EP);
    
-- Rellenar buffer con mensaje INIT y enviar al servidor
    CM.Message_Type'Output(Buffer'Access, CM.Init);
    LLU.End_Point_Type'Output(Buffer'Access, Client_EP);
    ASU.Unbounded_String'Output(Buffer'Access, Nick);
    
    LLU.Send(Server_EP, Buffer'Access);
    
    if ASU.To_String(Nick) = "reader" then
-- MODO LECTOR
        loop
            
            LLU.Reset(Buffer);
            LLU.Receive(Client_EP, Buffer'Access, 1000.0, Expired);

            Mess := CM.Message_Type'Input(Buffer'Access);
       		Nick := ASU.Unbounded_String'Input(Buffer'Access);
       		Text := ASU.Unbounded_String'Input(Buffer'Access);
			Ada.Text_IO.Put(ASCII.LF & ASU.To_String(Nick) & ": " & 
			                ASU.To_String(Text));
        
        end loop;
        
    else
-- MODO ESCRITOR
        Ada.Text_IO.New_Line(1);
        while ASU.To_String(Text) /= ".quit" loop
            
            LLU.Reset(Buffer);
            
            Ada.Text_IO.Put("Message: ");
            Text := ASU.To_Unbounded_String(Ada.Text_IO.Get_Line);
-- Rellenar buffer con mensaje Writer y enviar al servidor
            if ASU.To_String(Text) /= ".quit" then
                
                CM.Message_Type'Output(Buffer'Access, CM.Writer);
                LLU.End_Point_Type'Output(Buffer'Access, Client_EP);
                ASU.Unbounded_String'Output(Buffer'Access, Text);
                
                LLU.Send(Server_EP, Buffer'Access);
            
            end if;
        
        end loop;
        
    end if;

-- termina Lower_Layer_UDP
    LLU.Finalize;

exception
    
    when Usage_Error =>

        Ada.Text_IO.Put_Line(ASCII.LF & "Usage: " & ACL.Command_Name & 
                             " + <dir_ip> + <port> + <mode>");
        LLU.Finalize;
    
    when Constraint_Error =>

        Ada.Text_IO.Put_Line(ASCII.LF & "The port must be an integer");
        LLU.Finalize;
            
    when Ex:others =>
    
        Ada.Text_IO.Put_Line(ASCII.LF & "Excepción imprevista: " &
                             Ada.Exceptions.Exception_Name(Ex) & " en: " &
                             Ada.Exceptions.Exception_Message(Ex));
        LLU.Finalize;

end Chat_Client;
