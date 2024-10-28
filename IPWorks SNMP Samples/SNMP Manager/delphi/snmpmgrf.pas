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
unit snmpmgrf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ipncore, ipntypes, ipnsnmpmgr;

type
  TFormSnmpmgr = class(TForm)
    Label1: TLabel;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    lvwAgentList: TListView;
    bFind: TButton;
    bAdd: TButton;
    txtAddIPAddress: TEdit;
    txtAddSysName: TEdit;
    Label2: TLabel;
    cboVersion: TComboBox;
    gAuthParams: TGroupBox;
    Label3: TLabel;
    txtUser: TEdit;
    txtAPassword: TEdit;
    Label4: TLabel;
    txtEPassword: TEdit;
    Label5: TLabel;
    lvwResults: TListView;
    lstEvents: TListBox;
    snmpmgr1: Tipnsnmpmgr;
    function oidToName(oid:String) : String;
    procedure cboVersionChange(Sender: TObject);
    procedure bAddClick(Sender: TObject);
    procedure bFindClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure logEvents(msg: AnsiString);
    procedure snmpmgr1Report(Sender: TObject; RequestId,
      SNMPVersion: Integer; const Community, User: String;
      SecurityLevel: Integer; const SourceAddress: String; SourcePort,
      ErrorIndex, ErrorStatus: Integer; const ErrorDescription: String);
    procedure snmpmgr1PacketTrace(Sender: TObject; Packet: String;
      PacketB: TBytes; Direction: Integer; const PacketAddress: String;
      PacketPort: Integer);
    procedure snmpmgr1BadPacket(Sender: TObject; Packet: String;
      PacketB: TBytes; const SourceAddress: String; SourcePort,
      ErrorCode: Integer; const ErrorDescription: String; var Report: Boolean);
    procedure snmpmgr1Response(Sender: TObject; RequestId,
      SNMPVersion: Integer; const Community, User: String;
      SecurityLevel: Integer; const SourceAddress: String; SourcePort,
      ErrorIndex, ErrorStatus: Integer; const ErrorDescription: String);
    procedure snmpmgr1Error(Sender: TObject; ErrorCode: Integer;
      const Description: String);
    procedure lvwAgentListSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    { Private declarations }
        broadcastId: integer;
  public
    { Public declarations }
  end;

var
  FormSnmpmgr: TFormSnmpmgr;

implementation

{$R *.DFM}

Function TFormSnmpmgr.oidToName(oid:String) : String;
begin

    if (oid = '1.3.6.1.2.1.1.1.0') then Result := 'sysDescr'
    else if (oid = '1.3.6.1.2.1.1.2.0') then Result := 'sysObjectID'
    else if (oid = '1.3.6.1.2.1.1.3.0') then Result := 'sysUpTime'
    else if (oid = '1.3.6.1.2.1.1.4.0') then Result := 'sysContact'
    else if (oid = '1.3.6.1.2.1.1.5.0') then Result := 'sysName'
    else if (oid = '1.3.6.1.2.1.1.6.0') then Result := 'sysLocation'
    else if (oid = '1.3.6.1.2.1.1.7.0') then Result := 'sysServices'
    else if (oid = '1.3.6.1.6.3.1.1.5.1') then Result := 'coldStart'
    else if (oid = '1.3.6.1.6.3.1.1.5.2') then Result := 'warmStart'
    else if (oid = '1.3.6.1.6.3.1.1.5.3') then Result := 'linkDown'
    else if (oid = '1.3.6.1.6.3.1.1.5.4') then Result := 'linkUp'
    else if (oid = '1.3.6.1.6.3.1.1.5.5') then Result := 'authenticationFailure'
    else Result := oid;
end;

procedure TFormSnmpmgr.cboVersionChange(Sender: TObject);
begin
    if (cboVersion.Text = 'v1') then snmpmgr1.SNMPVersion := snmpverV1
    else if (cboVersion.Text = 'v2c') then snmpmgr1.SNMPVersion := snmpverV2c
    else if (cboVersion.Text = 'v3') then snmpmgr1.SNMPVersion := snmpverV3;
    gAuthParams.Enabled := (cboVersion.Text = 'v3');
end;

procedure TFormSnmpmgr.bAddClick(Sender: TObject);
begin
      If (txtAddIPAddress.Text <> '') Then begin
        lvwAgentList.Items.Add();
        lvwAgentList.Items[lvwAgentList.Items.Count-1].Caption := txtAddIPAddress.Text;
        lvwAgentList.Items[lvwAgentList.Items.Count - 1].SubItems.Add(txtAddsysName.Text);
        end
      Else
        MessageDlg('Please Supply a valid IP Address for the manager', mtError, [mbOk], 0);
      txtAddIPAddress.Text := '';
      txtAddsysName.Text := '';
end;

procedure TFormSnmpmgr.bFindClick(Sender: TObject);
var version:TipnsnmpmgrSNMPVersions;
begin
      version := snmpmgr1.SNMPVersion;
      lvwAgentList.Items.Clear();

      //bind to only one address:
      If snmpmgr1.Active = False Then
        snmpmgr1.LocalHost := snmpmgr1.LocalHost;

      snmpmgr1.Timeout := 0;
      snmpmgr1.ObjCount := 1;
      snmpmgr1.ObjId[0] := '1.3.6.1.2.1.1.5.0'; //sysName
      snmpmgr1.SNMPVersion := snmpverV2c;
      snmpmgr1.RemoteHost := '255.255.255.255'; //broadcast
      broadcastId := snmpmgr1.RequestId;
      try
        snmpmgr1.SendGetRequest();
      except on E:Exception do
        MessageDlg(E.Message, mtError, [mbOk], 0);
      end;
      //return to default values
      snmpmgr1.Timeout := 10;
      snmpmgr1.SNMPVersion := version;
end;

procedure TFormSnmpmgr.FormCreate(Sender: TObject);
begin
broadcastId := 0;
end;

procedure TFormSnmpmgr.logEvents(msg: AnsiString);
begin
  lstEvents.Items.Add(msg);
  lstEvents.ItemIndex := lstEvents.Items.Count - 1;
end;

procedure TFormSnmpmgr.snmpmgr1Report(Sender: TObject; RequestId,
  SNMPVersion: Integer; const Community, User: String;
  SecurityLevel: Integer; const SourceAddress: String; SourcePort,
  ErrorIndex, ErrorStatus: Integer; const ErrorDescription: String);
begin
        logEvents ('Report');
end;

procedure TFormSnmpmgr.snmpmgr1PacketTrace(Sender: TObject; Packet: String;
  PacketB: TBytes; Direction: Integer; const PacketAddress: String;
  PacketPort: Integer);
begin
      Case (Direction) of
        1: logEvents ('Packet received from: ' + PacketAddress + ':' + IntToStr(PacketPort));
        2: logEvents ('Packet sent to: ' + PacketAddress + ':' + IntToStr(PacketPort));
      end;
end;

procedure TFormSnmpmgr.snmpmgr1BadPacket(Sender: TObject; Packet: String;
  PacketB: TBytes; const SourceAddress: String; SourcePort, ErrorCode: Integer;
  const ErrorDescription: String; var Report: Boolean);
begin
        logEvents ('BadPacket');
end;

procedure TFormSnmpmgr.snmpmgr1Response(Sender: TObject; RequestId,
  SNMPVersion: Integer; const Community, User: String;
  SecurityLevel: Integer; const SourceAddress: String; SourcePort,
  ErrorIndex, ErrorStatus: Integer; const ErrorDescription: String);
begin
        //this handles the "Find" broadcasts
        logEvents ('Response');
        If (RequestId = broadcastId) And (snmpmgr1.ObjCount >= 1) And (snmpmgr1.ObjId[0] = '1.3.6.1.2.1.1.5.0') Then begin//sysName
           lvwAgentList.Items.Add();
           lvwAgentList.Items[lvwAgentList.Items.Count-1].Caption := SourceAddress;
           lvwAgentList.Items[lvwAgentList.Items.Count-1].SubItems.Add(snmpmgr1.ObjValue[0]);
        end;
end;

procedure TFormSnmpmgr.snmpmgr1Error(Sender: TObject; ErrorCode: Integer;
  const Description: String);
begin
        logEvents ('Error: ' + Description);
end;

procedure TFormSnmpmgr.lvwAgentListSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var i:integer;
begin
        if (lvwAgentList.ItemIndex < 0) then Exit;

        snmpmgr1.RemoteHost := lvwAgentList.Selected.Caption;
        lvwResults.Items.Clear();

        //SNMPv3 requires Discovery
        If (snmpmgr1.SNMPVersion = snmpverV3) Then begin
            snmpmgr1.User := txtUser.Text;
            snmpmgr1.AuthenticationPassword := txtAPassword.Text;
            snmpmgr1.EncryptionPassword := txtEPassword.Text;
            //discover the agent first to retrieve its engineid
            try
            snmpmgr1.Discover();
            except on E:Exception do begin
                MessageDlg(E.Message, mtError, [mbOk], 0);
                Exit;
                end;
            end;
        end;

        snmpmgr1.ObjCount := 1;
        snmpmgr1.ObjId[0] := '1.3.6.1.2.1.1.1';
        snmpmgr1.ObjType[0] := otOctetString;
        snmpmgr1.ObjValue[0] := '';

        For i := 1 To 6 do begin
            try
                snmpmgr1.SendGetNextRequest();
            except on E:Exception do begin
                MessageDlg(E.Message, mtError, [mbOk], 0);
                Exit;
                end;
            end;
            lvwResults.Items.Add();
            lvwResults.Items[lvwResults.Items.Count-1].Caption := snmpmgr1.ObjId[0];
            lvwResults.Items[lvwResults.Items.Count-1].SubItems.Add(oidToName(snmpmgr1.ObjId[0]));
            If (snmpmgr1.ErrorStatus = 0) Then lvwResults.Items[lvwResults.Items.Count-1].SubItems.Add(snmpmgr1.ObjValue[0])
            Else lvwResults.Items[lvwResults.Items.Count-1].SubItems.Add(snmpmgr1.ErrorDescription + ' [' + IntToStr(snmpmgr1.ErrorStatus) + ']')
        end;
end;

end.

