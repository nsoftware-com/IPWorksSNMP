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
unit snmpwalkf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ipncore, ipntypes, ipnsnmpmgr;

type
  TFormSnmpwalk = class(TForm)
    Label1: TLabel;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    txtAgent: TEdit;
    txtUser: TEdit;
    txtAuthPass: TEdit;
    txtEncPass: TEdit;
    cboAuthAlg: TComboBox;
    cboEncAlg: TComboBox;
    txtOID: TEdit;
    btnWalk: TButton;
    lvwObjects: TListView;
    cboSNMPVersion: TComboBox;
    SNMPMgr1: TipnSNMPMgr;
    procedure FormCreate(Sender: TObject);
    procedure cboSNMPVersionChange(Sender: TObject);
    procedure btnWalkClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormSnmpwalk: TFormSnmpwalk;

implementation

{$R *.dfm}

procedure TFormSnmpwalk.btnWalkClick(Sender: TObject);
var i: Integer;
begin
  try
    Screen.Cursor := crHourGlass;
    lvwObjects.Items.Clear;

    SNMPMgr1.SNMPVersion := TipnSNMPMgrSNMPVersions(cboSNMPVersion.ItemIndex+1);
    SNMPMgr1.RemoteHost := txtAgent.Text;

    if(cboSNMPVersion.ItemIndex = 2) then begin  //SNMPv3
      SNMPMgr1.EncryptionAlgorithm := TipnsnmpmgrEncryptionAlgorithms(cboEncAlg.ItemIndex);
      SNMPMgr1.AuthenticationProtocol := TipnsnmpmgrAuthenticationProtocols(cboAuthAlg.ItemIndex);
      SNMPMgr1.EncryptionPassword := txtEncPass.Text;
      SNMPMgr1.AuthenticationPassword := txtAuthPass.Text;
    end;

    SNMPMgr1.Walk(txtOID.Text);

    for i:=0 to SNMPMgr1.ObjCount -1 do begin
      lvwObjects.Items.Add();
      lvwObjects.Items[lvwObjects.Items.Count -1].Caption := SNMPMgr1.ObjId[i];
      lvwObjects.Items[lvwObjects.Items.Count -1].SubItems.Add(SNMPMgr1.ObjTypeString[i]);
      lvwObjects.Items[lvwObjects.Items.Count -1].SubItems.Add(SNMPMgr1.ObjValue[i]);
    end;

  Except on E:Exception do
    MessageDlg('Exception: ' + E.Message, mtInformation, [mbOk], 0);
  end;
  Screen.Cursor := crDefault;
end;

procedure TFormSnmpwalk.cboSNMPVersionChange(Sender: TObject);
begin
if(cboSNMPVersion.ItemIndex = 2) then begin //SNMPv3
  cboEncAlg.Enabled := True;
  cboAuthAlg.Enabled := True;
  txtUser.Enabled := True;
  txtAuthPass.Enabled := True;
  txtEncPass.Enabled := True;
end
else begin
  cboEncAlg.Enabled := False;
  cboAuthAlg.Enabled := False;
  txtUser.Enabled := False;
  txtAuthPass.Enabled := False;
  txtEncPass.Enabled := False;
end;
end;

procedure TFormSnmpwalk.FormCreate(Sender: TObject);
begin
cboSNMPVersion.ItemIndex := 1;
cboEncAlg.ItemIndex := 0;
cboAuthAlg.ItemIndex := 0;
end;

end.

