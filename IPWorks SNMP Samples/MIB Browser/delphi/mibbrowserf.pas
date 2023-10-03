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
unit mibbrowserf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ipncore, ipntypes, ipnmibbrowser, ComCtrls;

type
    PNodeData = ^TNodeData;
    TNodeData = record
        Oid : String;
    end;

    TFormMibbrowser = class(TForm)
    MibBrowser1: TipnMibBrowser;
    btnLoadMibs: TButton;
    Label1: TLabel;
    tvwMibNodes: TTreeView;
    lvwProperties: TListView;
    txtDescription: TMemo;
    tvwTrapNodes: TTreeView;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    OpenDialog1: TOpenDialog;
    procedure btnLoadMibsClick(Sender: TObject);
    procedure MibBrowser1MibNode(Sender: TObject; const NodeLabel,
      NodeOid: String; NodeType: Integer; const NodeTypeString: String;
      NodeSyntax: Integer; const NodeSyntaxString: String;
      NodeAccess: Integer; const NodeIndex, NodeParentName,
      NodeDescription, NodeModuleName, NodeFileName: String);
    procedure MibBrowser1TrapNode(Sender: TObject; const TrapName: String;
      SpecificTrap: Integer; const TrapEnterprise, TrapVariables,
      TrapDescription, TrapReferences, TrapModuleName,
      TrapFileName: String);
    procedure tvwTrapNodesClick(Sender: TObject);
    procedure tvwMibNodesClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure CreateTree(NodeOid: String; strSeparator: String; strParent: String; NodeLabel:String);
    procedure ShowNodeProperties(NodeOid: String);
    function FindNode(Node: TTreeNode; strOid: String): TTreeNode;
  end;

var
  FormMibbrowser: TFormMibbrowser;
  strTreeFlag: String;
  strParent: String;

implementation

{$R *.dfm}

procedure TFormMibbrowser.btnLoadMibsClick(Sender: TObject);
var
nodeData: PNodeData;
begin

        tvwMibNodes.Items.Clear;
        tvwTrapNodes.Items.Clear;

        with  tvwMibNodes.Items.AddFirst(nil,'MIB') do
        begin
                Selected := true;
                strTreeFlag := Text;
                MakeVisible;
                New(nodeData);
                nodeData.Oid := 'root';
                Data := nodeData;
        end;

        with tvwTrapNodes.Items.AddFirst(nil,'Traps') do
        begin
                Selected := true;
                MakeVisible;
                New(nodeData);
                nodeData.Oid := 'root';
                Data := nodeData;                
        end;
        
        if OpenDialog1.Execute() then
                MibBrowser1.LoadMib(OpenDialog1.FileName);
                MibBrowser1.ListSuccessors;
                Mibbrowser1.ListTraps;
        begin

        end;
end;

procedure TFormMibbrowser.MibBrowser1MibNode(Sender: TObject; const NodeLabel,
  NodeOid: String; NodeType: Integer; const NodeTypeString: String;
  NodeSyntax: Integer; const NodeSyntaxString: String; NodeAccess: Integer;
  const NodeIndex, NodeParentName, NodeDescription, NodeModuleName,
  NodeFileName: String);
begin
  CreateTree(NodeOid, '.', tvwMibNodes.Selected.Text,NodeLabel);
end;

procedure TFormMibbrowser.MibBrowser1TrapNode(Sender: TObject;
  const TrapName: String; SpecificTrap: Integer; const TrapEnterprise,
  TrapVariables, TrapDescription, TrapReferences, TrapModuleName,
  TrapFileName: String);
var
i: Integer;
curNode : TTreeNode;
trapData: PNodeData;
begin
        New(trapData);
        for i:= 1 to tvwTrapNodes.Items.Count - 1 do
        begin
            if (tvwTrapNodes.Items[i].Text = TrapName) then Exit;
        end;

        trapData.Oid := TrapEnterprise + '.' + IntToStr(SpecificTrap);
        curNode := tvwTrapNodes.Items.AddChild(tvwTrapNodes.Selected, TrapName);
        curNode.MakeVisible;
        curNode.Data := trapData;
end;

procedure TFormMibbrowser.CreateTree(NodeOid: String; strSeparator: String; strParent: String; NodeLabel:String);
var
parentNode : TTreeNode;
tempNode : TTreeNode;
strNodes: TStringList;
tempData : PNodeData;
tempOid : AnsiString;
i: Integer;
begin

        strNodes := TStringList.Create;
        tvwMibNodes.Items[0].Selected := true; //Select the root node
        parentNode := tvwMibNodes.Items[0];

        //Now split the Oid
        strNodes.Delimiter := '.';
        strNodes.DelimitedText := NodeOid;

        tempOid := '';

        for i:= 0 to strNodes.Count -1 do
        begin
            //Special case for the first node
            if not (tempOid = '') then begin
                tempOid := tempOid + strSeparator + strNodes[i];
                end
            else begin
                tempOid := strNodes[i];
                end;

            tempNode := FindNode(parentNode,tempOid);

            if(tempNode = nil) then//The node wasn't found so add it to the current parent.
            begin
                New(tempData);
                tempData.Oid := NodeOid;
                tempNode := tvwMibNodes.Items.AddChild(parentNode,NodeLabel);
                tempNode.Data := tempData;
            end;

            parentNode := tempNode;
        end;
end;

procedure TFormMibbrowser.ShowNodeProperties(NodeOid: String);
begin
        MibBrowser1.SelectNode(NodeOid);
        lvwProperties.Items.Clear;

        lvwProperties.Items.Add;
        lvwProperties.Items[lvwProperties.Items.Count-1].Caption := 'Oid';
        lvwProperties.Items[lvwProperties.Items.Count-1].SubItems.Add(Mibbrowser1.NodeOid);

        lvwProperties.Items.Add;
        lvwProperties.Items[lvwProperties.Items.Count-1].Caption := 'Label';
        lvwProperties.Items[lvwProperties.Items.Count-1].SubItems.Add(Mibbrowser1.NodeLabel);

        lvwProperties.Items.Add;
        lvwProperties.Items[lvwProperties.Items.Count-1].Caption := 'NodeSyntax';
        lvwProperties.Items[lvwProperties.Items.Count-1].SubItems.Add(IntToStr(Integer(Mibbrowser1.NodeSyntax)));

        lvwProperties.Items.Add;
        lvwProperties.Items[lvwProperties.Items.Count-1].Caption := 'NodeSyntaxString';
        lvwProperties.Items[lvwProperties.Items.Count-1].SubItems.Add(Mibbrowser1.NodeSyntaxString);

        lvwProperties.Items.Add;
        lvwProperties.Items[lvwProperties.Items.Count-1].Caption := 'NodeType';
        lvwProperties.Items[lvwProperties.Items.Count-1].SubItems.Add(IntToStr(Integer(Mibbrowser1.NodeType)));

        lvwProperties.Items.Add;
        lvwProperties.Items[lvwProperties.Items.Count-1].Caption := 'NodeTypeString';
        lvwProperties.Items[lvwProperties.Items.Count-1].SubItems.Add(Mibbrowser1.NodeTypeString);

        lvwProperties.Items.Add;
        lvwProperties.Items[lvwProperties.Items.Count-1].Caption := 'Access';
        lvwProperties.Items[lvwProperties.Items.Count-1].SubItems.Add(IntToStr(Integer(Mibbrowser1.NodeAccess)));

        lvwProperties.Items.Add;
        lvwProperties.Items[lvwProperties.Items.Count-1].Caption := 'Index';
        lvwProperties.Items[lvwProperties.Items.Count-1].SubItems.Add(Mibbrowser1.NodeIndex);

        lvwProperties.Items.Add;
        lvwProperties.Items[lvwProperties.Items.Count-1].Caption := 'SubId';
        lvwProperties.Items[lvwProperties.Items.Count-1].SubItems.Add(IntToStr(Mibbrowser1.NodeSubId));

        lvwProperties.Items.Add;
        lvwProperties.Items[lvwProperties.Items.Count-1].Caption := 'ModuleName';
        lvwProperties.Items[lvwProperties.Items.Count-1].SubItems.Add(Mibbrowser1.NodeModuleName);

        lvwProperties.Items.Add;
        lvwProperties.Items[lvwProperties.Items.Count-1].Caption := 'FileName';
        lvwProperties.Items[lvwProperties.Items.Count-1].SubItems.Add(Mibbrowser1.NodeFileName);


        txtDescription.Text := MibBrowser1.NodeDescription;

end;

function TFormMibbrowser.FindNode(Node: TTreeNode; strOid: String): TTreeNode;
var
i : Integer;
key: TTreeNode;
nodeData : PNodeData;
begin
   key := nil;
   nodeData := Node.Data;
   if nodeData.Oid = strOid then
      Result := Node
   else if Node.Count = 0 then
      Result := nil
   else
   begin
      for i := 0 to Node.Count - 1 do
         if key = nil then
            key := FindNode(Node.Item[i], strOid);
      Result := key;
   end;
end;

procedure TFormMibbrowser.tvwTrapNodesClick(Sender: TObject);
var
trapData: PNodeData;
begin
        trapData := tvwTrapNodes.Selected.Data;
        if(trapData <> nil) and (trapData.Oid <> 'root') then
         ShowNodeProperties(trapData.Oid);
end;

procedure TFormMibbrowser.tvwMibNodesClick(Sender: TObject);
var
nodeData: PNodeData;
begin
        nodeData := tvwMibNodes.Selected.Data;
        if(nodeData <> nil) and (nodeData.Oid <> 'root') then
         ShowNodeProperties(nodeData.Oid);
end;

end.

