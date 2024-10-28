(*
 * IPWorks SNMP 2024 Delphi Edition - Sample Project
 *
 * This sample project demonstrates the usage of IPWorks SNMP in a 
 * simple, straightforward way. It is not intended to be a complete 
 * application. Error handling and other checks are simplified for clarity.
 *
 * www.nsoftware.com/ipworkssnmp
 *
 * This code is subject to the terms and conditions specified in the 
 * corresponding product license agreement which outlines the authorized 
 * usage and restrictions.
 *)

program mibbrowser;

uses
  Forms,
  mibbrowserf in 'mibbrowserf.pas' {FormMibbrowser};

begin
  Application.Initialize;

  Application.CreateForm(TFormMibbrowser, FormMibbrowser);
  Application.Run;
end.


         
