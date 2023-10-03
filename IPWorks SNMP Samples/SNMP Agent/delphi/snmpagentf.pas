(*
 * IPWorks SNMP 2022 Delphi Edition - Sample Project
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
unit snmpagentf;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, ipncore, ipntypes, ipnsnmpagent;

type
  TFormSnmpagent = class(TForm)
    TabControl1: TTabControl;
    Label1: TLabel;
    pnlObjectIds: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;                
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    txtEngineID: TEdit;
    txtOIDVal0: TEdit;
    txtOIDVal1: TEdit;
    txtOIDVal2: TEdit;
    txtOIDVal3: TEdit;
    txtOIDVal4: TEdit;
    txtOIDVal5: TEdit;
    bListen: TButton;
    GroupBox2: TGroupBox;
    lstEvents: TListBox;
    pnlUsers: TPanel;
    Label15: TLabel;
    cbAnonGet: TCheckBox;
    cbAnonSet: TCheckBox;
    GroupBox1: TGroupBox;
    lvwUsers: TListView;
    tbNewUser: TEdit;
    tbNewAuthPass: TEdit;
    tbNewEncPass: TEdit;
    cbNewGet: TCheckBox;
    cbNewSet: TCheckBox;
    bAdd: TButton;
    Remove: TButton;
    pnlTraps: TPanel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    cbTrapOID: TComboBox;
    txtRemHost: TEdit;
    rbv1: TRadioButton;
    rbv2c: TRadioButton;
    rbv3: TRadioButton;
    bSendTrap: TButton;
    gbAuthPriv: TGroupBox;
    Label13: TLabel;
    Label14: TLabel;
    Label9: TLabel;
    txtTrapUser: TEdit;
    txtTrapAPassword: TEdit;
    txtTrapEPassword: TEdit;
    Timer1: TTimer;
    snmpagent1: TipnSNMPAgent;
    procedure TabControl1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure bListenClick(Sender: TObject);
    procedure bAddClick(Sender: TObject);
    procedure RemoveClick(Sender: TObject);
    procedure bSendTrapClick(Sender: TObject);
    procedure pnlTrapsClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure txtEngineIDChange(Sender: TObject);
    procedure logEvents(msg: AnsiString);
    function CheckAccess(User: AnsiString; SecLevel: Integer; RequiredAccess: AnsiString): boolean;
    procedure snmpagent1Error(Sender: TObject; ErrorCode: Integer;
      const Description: String);
    procedure snmpagent1Report(Sender: TObject; RequestId,
      SNMPVersion: Integer; const Community, User: String;
      SecurityLevel: Integer; const SourceAddress: String; SourcePort,
      ErrorIndex, ErrorStatus: Integer; const ErrorDescription: String);
    procedure snmpagent1GetUserPassword(Sender: TObject; PasswordType: Integer;
      const User: string; var Password: string;
      var EncryptionAlgorithm: Integer);
    procedure snmpagent1GetBulkRequest(Sender: TObject; RequestId,
      MessageId, SNMPVersion: Integer; const Community, User: String;
      SecurityLevel: Integer; const SourceAddress: String; SourcePort,
      NonRepeaters, MaxRepetitions: Integer; var ErrorIndex,
      ErrorStatus: Integer; const ErrorDescription: String;
      var Respond: Boolean);
    procedure snmpagent1SetRequest(Sender: TObject; RequestId, MessageId,
      SNMPVersion: Integer; const Community, User: String;
      SecurityLevel: Integer; const SourceAddress: String;
      SourcePort: Integer; var ErrorIndex, ErrorStatus: Integer;
      const ErrorDescription: String; var Respond: Boolean);
    procedure snmpagent1GetRequest(Sender: TObject; RequestId, MessageId,
      SNMPVersion: Integer; const Community, User: String;
      SecurityLevel: Integer; const SourceAddress: String;
      SourcePort: Integer; var ErrorIndex, ErrorStatus: Integer;
      const ErrorDescription: String; var Respond: Boolean);
    procedure snmpagent1GetNextRequest(Sender: TObject; RequestId,
      MessageId, SNMPVersion: Integer; const Community, User: String;
      SecurityLevel: Integer; const SourceAddress: String;
      SourcePort: Integer; var ErrorIndex, ErrorStatus: Integer;
      const ErrorDescription: String; var Respond: Boolean);
    procedure SNMPAgent1BadPacket(Sender: TObject; Packet: string;
      PacketB: TArray<System.Byte>; const SourceAddress: string; SourcePort,
      ErrorCode: Integer; const ErrorDescription: string; var Report: Boolean);
    procedure SNMPAgent1DiscoveryRequest(Sender: TObject; EngineId: string;
      EngineIdB: TArray<System.Byte>; EngineBoots, EngineTime: Integer;
      const User: string; SecurityLevel: Integer; const SourceAddress: string;
      SourcePort: Integer; var Respond: Boolean);
    procedure SNMPAgent1PacketTrace(Sender: TObject; Packet: string;
      PacketB: TArray<System.Byte>; Direction: Integer;
      const PacketAddress: string; PacketPort: Integer);
 

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
   FormSnmpagent: TFormSnmpagent;

implementation

{$R *.DFM}

procedure TFormSnmpagent.TabControl1Change(Sender: TObject);
begin
        case TabControl1.TabIndex of
                0: pnlObjectIds.BringToFront();
                1: pnlTraps.BringToFront();
                2: pnlUsers.BringToFront();
        end;
end;

procedure TFormSnmpagent.FormCreate(Sender: TObject);
var i:integer;
begin
    snmpagent1.LocalEngineId := txtEngineId.Text;

    lvwUsers.Items.Add();
    lvwUsers.Items[lvwUsers.Items.Count-1].Caption := 'desuser';
    lvwUsers.Items[lvwUsers.Items.Count-1].SubItems.Add('des_password');
    lvwUsers.Items[lvwUsers.Items.Count-1].SubItems.Add('des_password');
    lvwUsers.Items[lvwUsers.Items.Count-1].SubItems.Add('YES');
    lvwUsers.Items[lvwUsers.Items.Count-1].SubItems.Add('YES');
    lvwUsers.Items.Add();
    lvwUsers.Items[lvwUsers.Items.Count-1].Caption := 'myuser';
    lvwUsers.Items[lvwUsers.Items.Count-1].SubItems.Add('my_password');
    lvwUsers.Items[lvwUsers.Items.Count-1].SubItems.Add('');
    lvwUsers.Items[lvwUsers.Items.Count-1].SubItems.Add('YES');
    lvwUsers.Items[lvwUsers.Items.Count-1].SubItems.Add('NO');
    lvwUsers.Items.Add();
    lvwUsers.Items[lvwUsers.Items.Count-1].Caption := 'plainuser';
    lvwUsers.Items[lvwUsers.Items.Count-1].SubItems.Add('');
    lvwUsers.Items[lvwUsers.Items.Count-1].SubItems.Add('');
    lvwUsers.Items[lvwUsers.Items.Count-1].SubItems.Add('YES');
    lvwUsers.Items[lvwUsers.Items.Count-1].SubItems.Add('NO');
    
    //Add the default users
    For i := 0 To lvwUsers.Items.Count-1 do begin
        snmpagent1.AddUser(lvwUsers.Items[i].Caption, 1, lvwUsers.Items[i].SubItems[0], 1, lvwUsers.Items[i].SubItems[1]);
    end;

end;

procedure TFormSnmpagent.bListenClick(Sender: TObject);
begin
        //Toggle listening, initialize
        //bind to only one address:
        if snmpagent1.Active = false then begin
                snmpagent1.LocalHost := snmpagent1.LocalHost;
        end;
        snmpagent1.Active := Not snmpagent1.Active;
        Timer1.Enabled := snmpagent1.Active;
        txtOIDVal2.Text := '0';
        If snmpagent1.Active Then
                bListen.Caption := 'Stop &Listening'
        Else bListen.Caption := 'Start &Listening';

end;

procedure TFormSnmpagent.bAddClick(Sender: TObject);
begin
    //Adds a user to the listview on the "user mgmt" tab:
    lvwUsers.Items.Add();
    lvwUsers.Items[lvwUsers.Items.Count-1].Caption := tbNewUser.Text;
    lvwUsers.Items[lvwUsers.Items.Count-1].SubItems.Add(tbNewAuthPass.Text);
    lvwUsers.Items[lvwUsers.Items.Count-1].SubItems.Add(tbNewEncPass.Text);

    If (cbNewGet.Checked) Then lvwUsers.Items[lvwUsers.Items.Count-1].SubItems.Add('YES')
    else lvwUsers.Items[lvwUsers.Items.Count-1].SubItems.Add('NO');
    If (cbNewSet.Checked) Then lvwUsers.Items[lvwUsers.Items.Count-1].SubItems.Add('YES')
    else lvwUsers.Items[lvwUsers.Items.Count-1].SubItems.Add('NO');
    //Adds the user to the snmpagentf components internal cache:
    snmpagent1.AddUser(tbNewUser.Text, 1, tbNewAuthPass.Text, 1, tbNewEncPass.Text);
end;

procedure TFormSnmpagent.RemoveClick(Sender: TObject);
begin
        If lvwUsers.SelCount > 0 Then begin
            //Removes the user from the snmpagentf components internal cache:
            snmpagent1.RemoveUser (lvwUsers.Selected.Caption);
            //Removes selected user from the listview on the "user mgmt" tab
            lvwUsers.Items.Delete (lvwUsers.Selected.Index)
        end;
end;


procedure TFormSnmpagent.SNMPAgent1BadPacket(Sender: TObject; Packet: string;
  PacketB: TArray<System.Byte>; const SourceAddress: string; SourcePort,
  ErrorCode: Integer; const ErrorDescription: string; var Report: Boolean);
begin
        logEvents ('BadPacket');
end;

procedure TFormSnmpagent.SNMPAgent1DiscoveryRequest(Sender: TObject;
  EngineId: string; EngineIdB: TArray<System.Byte>; EngineBoots,
  EngineTime: Integer; const User: string; SecurityLevel: Integer;
  const SourceAddress: string; SourcePort: Integer; var Respond: Boolean);
begin
        logEvents ('DiscoveryRequest');
end;

procedure TFormSnmpagent.bSendTrapClick(Sender: TObject);
begin
        If (rbV1.Checked) Then
          snmpagent1.SNMPVersion := snmpverV1
        else if (rbV2c.Checked) Then
          snmpagent1.SNMPVersion := snmpverV2c
        else If (rbV3.Checked) Then
            snmpagent1.SNMPVersion := snmpverV3;
        snmpagent1.Reset();
        If (rbV3.Checked And (txtTrapUser.Text <> '')) Then
            snmpagent1.SendSecureTrap(txtRemHost.Text, cbTrapOID.Text, txtTrapUser.Text, 1, txtTrapAPassword.Text, 1, txtTrapEPassword.Text)
        else
            snmpagent1.SendTrap(txtRemHost.Text, cbTrapOID.Text);
end;

procedure TFormSnmpagent.pnlTrapsClick(Sender: TObject);
begin
    gbAuthPriv.Enabled := rbV3.Checked;
end;

procedure TFormSnmpagent.Timer1Timer(Sender: TObject);
begin
    txtOIDVal2.Text := IntToStr(snmpagent1.SysUpTime);
end;

function TFormSnmpagent.CheckAccess(User: AnsiString; SecLevel: Integer; RequiredAccess: AnsiString): Boolean;
var i:integer;
    Access:boolean;
begin
    Access := false;
    Case SecLevel of
        0:  //anonymous and unauthenticated users
            If (requiredAccess = 'GET') and cbAnonGet.Checked then Access := true
            else If (requiredAccess = 'SET') and cbAnonSet.Checked then Access := true;
        1:  //authenticated users
            For i := 0 To lvwUsers.Items.Count-1 do begin
                If lvwUsers.Items[i].Caption = User Then begin
                    If requiredAccess = 'GET' Then Access := lvwUsers.Items[i].SubItems[2] = 'YES';
                    If requiredAccess = 'SET' Then Access := lvwUsers.Items[i].SubItems[3] = 'YES';
                end;
            end;
        2:  //auth/priv users
            For i := 0 To lvwUsers.Items.Count-1 do begin
                If lvwUsers.Items[i].Caption = User Then begin
                    If requiredAccess = 'GET' Then Access := lvwUsers.Items[i].SubItems[2] = 'YES';
                    If requiredAccess = 'SET' Then Access := lvwUsers.Items[i].SubItems[3] = 'YES';
                end;
            end;
    end;
    Result :=access;
end;

procedure TFormSnmpagent.logEvents(msg: AnsiString);
begin
  lstEvents.Items.Add(msg);
  lstEvents.ItemIndex := lstEvents.Items.Count - 1;
end;

procedure TFormSnmpagent.txtEngineIDChange(Sender: TObject);
begin
snmpagent1.LocalEngineId := txtEngineId.Text;
end;

procedure TFormSnmpagent.snmpagent1Error(Sender: TObject; ErrorCode: Integer;
  const Description: String);
begin
        logEvents ('Error: ' + Description);
end;

procedure TFormSnmpagent.snmpagent1GetBulkRequest(Sender: TObject;
  RequestId, MessageId, SNMPVersion: Integer; const Community,
  User: String; SecurityLevel: Integer; const SourceAddress: String;
  SourcePort, NonRepeaters, MaxRepetitions: Integer; var ErrorIndex,
  ErrorStatus: Integer; const ErrorDescription: String;
  var Respond: Boolean);
begin
                logEvents ('GetBulkRequest');
end;

procedure TFormSnmpagent.snmpagent1GetUserPassword(Sender: TObject;
  PasswordType: Integer; const User: string; var Password: string;
  var EncryptionAlgorithm: Integer);
begin
        logEvents ('GetUserPassword: ' + User + ' password: ' + Password + ' passtype: ' + IntToStr(PasswordType));
end;

procedure TFormSnmpagent.SNMPAgent1PacketTrace(Sender: TObject; Packet: string;
  PacketB: TArray<System.Byte>; Direction: Integer; const PacketAddress: string;
  PacketPort: Integer);
begin
      Case (Direction) of
        1: logEvents ('Packet received from: ' + PacketAddress + ':' + IntToStr(PacketPort));
        2: logEvents ('Packet sent to: ' + PacketAddress + ':' + IntToStr(PacketPort));
      end;
end;

procedure TFormSnmpagent.snmpagent1Report(Sender: TObject; RequestId,
  SNMPVersion: Integer; const Community, User: String;
  SecurityLevel: Integer; const SourceAddress: String; SourcePort,
  ErrorIndex, ErrorStatus: Integer; const ErrorDescription: String);
begin
        logEvents ('Report');
end;




procedure TFormSnmpagent.snmpagent1SetRequest(Sender: TObject; RequestId,
  MessageId, SNMPVersion: Integer; const Community, User: String;
  SecurityLevel: Integer; const SourceAddress: String; SourcePort: Integer;
  var ErrorIndex, ErrorStatus: Integer; const ErrorDescription: String;
  var Respond: Boolean);
 var i:integer;
  begin
        logEvents ('SetRequest');
        If Not CheckAccess(User, SecurityLevel, 'GET') Then Exit;

        //If we get this far, we are answering the request.  By default, Respond is false,
        //meaning do NOT answer the request.  So we'll change this to True, so that a response
        //is automatically sent after the code in the event is completed.
        Respond := True;

        //this may not be the most efficient method, however for the purposes of
        //a clear demo, we'll loop through and set all of the objtypes and objvalues for
        //the response...
        For i := 0 To snmpagent1.ObjCount-1 do begin
            if (snmpagent1.ObjId[i] = '1.3.6.1.2.1.1.1.0') then begin
                 txtOIDVal0.Text := snmpagent1.ObjValue[i];
                 end
            else if (snmpagent1.ObjId[i] = '1.3.6.1.2.1.1.2.0') then begin
                 txtOIDVal1.Text := snmpagent1.ObjValue[i];
                 end
            else if (snmpagent1.ObjId[i] = '1.3.6.1.2.1.1.3.0') then begin
                 txtOIDVal2.Text := snmpagent1.ObjValue[i];
                 end
            else if (snmpagent1.ObjId[i] = '1.3.6.1.2.1.1.4.0') then begin
                 txtOIDVal3.Text := snmpagent1.ObjValue[i];
                 end
            else if (snmpagent1.ObjId[i] = '1.3.6.1.2.1.1.5.0') then begin
                 txtOIDVal4.Text := snmpagent1.ObjValue[i];
                 end
            else ErrorStatus := 2;  //No such name
        end; //end for

end;

procedure TFormSnmpagent.snmpagent1GetRequest(Sender: TObject; RequestId,
  MessageId, SNMPVersion: Integer; const Community, User: String;
  SecurityLevel: Integer; const SourceAddress: String; SourcePort: Integer;
  var ErrorIndex, ErrorStatus: Integer; const ErrorDescription: String;
  var Respond: Boolean);
var i:integer;
begin
        logEvents ('GetRequest');
        If Not CheckAccess(User, SecurityLevel, 'GET') Then Exit;

        //If we get this far, we are answering the request.  By default, Respond is false,
        //meaning do NOT answer the request.  So we'll change this to True, so that a response
        //is automatically sent after the code in the event is completed.
        Respond := True;

        //this may not be the most efficient method, however for the purposes of
        //a clear demo, we'll loop through and set all of the objtypes and objvalues for
        //the response...
        For i := 0 To snmpagent1.ObjCount-1 do begin
            if (snmpagent1.ObjId[i] = '1.3.6.1.2.1.1.1') then begin
                 snmpagent1.ObjId[i] := '1.3.6.1.2.1.1.1.0';
                 snmpagent1.ObjValue[i] := txtOIDVal0.Text;
                 snmpagent1.ObjType[i] := otOctetString;
                 end
            else if (snmpagent1.ObjId[i] = '1.3.6.1.2.1.1.1.0') then begin
                 snmpagent1.ObjId[i] := '1.3.6.1.2.1.1.1.0';
                 snmpagent1.ObjValue[i] := txtOIDVal0.Text;
                 snmpagent1.ObjType[i] := otOctetString;
                 end
            else if (snmpagent1.ObjId[i] = '1.3.6.1.2.1.1.2.0') then begin
                 snmpagent1.ObjId[i] := '1.3.6.1.2.1.1.2.0';
                 snmpagent1.ObjValue[i] := txtOIDVal1.Text;
                 snmpagent1.ObjType[i] := otObjectId;
                 end
            else if (snmpagent1.ObjId[i] = '1.3.6.1.2.1.1.3.0') then begin
                 snmpagent1.ObjId[i] := '1.3.6.1.2.1.1.3.0';
                 snmpagent1.ObjValue[i] := txtOIDVal2.Text;
                 snmpagent1.ObjType[i] := otTimeTicks;
                 end
            else if (snmpagent1.ObjId[i] = '1.3.6.1.2.1.1.4.0') then begin
                 snmpagent1.ObjId[i] := '1.3.6.1.2.1.1.4.0';
                 snmpagent1.ObjValue[i] := txtOIDVal3.Text;
                 snmpagent1.ObjType[i] := otOctetString;
                 end
            else if (snmpagent1.ObjId[i] = '1.3.6.1.2.1.1.5.0') then begin
                 snmpagent1.ObjId[i] := '1.3.6.1.2.1.1.5.0';
                 snmpagent1.ObjValue[i] := txtOIDVal4.Text;
                 snmpagent1.ObjType[i] := otOctetString;
                 end
            else ErrorStatus := 2;  //No such name
        end; //end for
end;

procedure TFormSnmpagent.snmpagent1GetNextRequest(Sender: TObject;
  RequestId, MessageId, SNMPVersion: Integer; const Community,
  User: String; SecurityLevel: Integer; const SourceAddress: String;
  SourcePort: Integer; var ErrorIndex, ErrorStatus: Integer;
  const ErrorDescription: String; var Respond: Boolean);
var i:Integer;
begin
        logEvents ('GetNextRequest');
        If Not CheckAccess(User, SecurityLevel, 'GET') Then Exit;

        //If we get this far, we are answering the request.  By default, Respond is false,
        //meaning do NOT answer the request.  So we'll change this to True, so that a response
        //is automatically sent after the code in the event is completed.
        Respond := True;

        //this may not be the most efficient method, however for the purposes of
        //a clear demo, we'll loop through and set all of the objtypes and objvalues for
        //the response...
        For i := 0 To snmpagent1.ObjCount-1 do begin
            if (snmpagent1.ObjId[i] = '1.3.6.1.2.1.1.1') then begin
                 snmpagent1.ObjId[i] := '1.3.6.1.2.1.1.1.0';
                 snmpagent1.ObjValue[i] := txtOIDVal0.Text;
                 snmpagent1.ObjType[i] := otOctetString;
                 end
            else if (snmpagent1.ObjId[i] = '1.3.6.1.2.1.1.1.0') then begin
                 snmpagent1.ObjId[i] := '1.3.6.1.2.1.1.2.0';
                 snmpagent1.ObjValue[i] := txtOIDVal1.Text;
                 snmpagent1.ObjType[i] := otObjectId;
                 end
            else if (snmpagent1.ObjId[i] = '1.3.6.1.2.1.1.2.0') then begin
                 snmpagent1.ObjId[i] := '1.3.6.1.2.1.1.3.0';
                 snmpagent1.ObjValue[i] := txtOIDVal2.Text;
                 snmpagent1.ObjType[i] := otTimeTicks;
                 end
            else if (snmpagent1.ObjId[i] = '1.3.6.1.2.1.1.3.0') then begin
                 snmpagent1.ObjId[i] := '1.3.6.1.2.1.1.4.0';
                 snmpagent1.ObjValue[i] := txtOIDVal3.Text;
                 snmpagent1.ObjType[i] := otOctetString;
                 end
            else if (snmpagent1.ObjId[i] = '1.3.6.1.2.1.1.4.0') then begin
                 snmpagent1.ObjId[i] := '1.3.6.1.2.1.1.5.0';
                 snmpagent1.ObjValue[i] := txtOIDVal4.Text;
                 snmpagent1.ObjType[i] := otOctetString;
                 end
            else if (snmpagent1.ObjId[i] = '1.3.6.1.2.1.1.5.0') then begin
                 snmpagent1.ObjId[i] := '1.3.6.1.2.1.1.6.0';
                 snmpagent1.ObjValue[i] := txtOIDVal5.Text;
                 snmpagent1.ObjType[i] := otOctetString;
                 end
            else ErrorStatus := 2;  //No such name
        end; //end for
end;

end.

