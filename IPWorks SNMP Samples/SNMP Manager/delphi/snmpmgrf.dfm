object FormSnmpmgr: TFormSnmpmgr
  Left = 232
  Top = 63
  Caption = 'SNMP Manager Demo'
  ClientHeight = 709
  ClientWidth = 617
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    617
    709)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 2
    Top = 2
    Width = 585
    Height = 26
    Caption = 
      'This demo shows how to create a basic SNMP manager.  To start, c' +
      'lick '#39'Find Agents'#39' to find the SNMP agents on your LAN, or add a' +
      'n SNMP agent at a specified IP address.  Then select the Agent i' +
      'n the '#39'Agent List Box'#39' to retrieve system values.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object GroupBox1: TGroupBox
    Left = 2
    Top = 30
    Width = 408
    Height = 201
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Agent List'
    TabOrder = 0
    DesignSize = (
      408
      201)
    object lvwAgentList: TListView
      Left = 90
      Top = 10
      Width = 310
      Height = 151
      Anchors = [akLeft, akTop, akRight]
      Columns = <
        item
          Caption = 'IP Address'
          Width = 150
        end
        item
          Caption = 'System Name'
          Width = 155
        end>
      TabOrder = 0
      ViewStyle = vsReport
      OnSelectItem = lvwAgentListSelectItem
    end
    object bFind: TButton
      Left = 8
      Top = 138
      Width = 73
      Height = 21
      Caption = 'Find'
      TabOrder = 1
      OnClick = bFindClick
    end
    object bAdd: TButton
      Left = 8
      Top = 168
      Width = 73
      Height = 21
      Caption = 'Add'
      TabOrder = 2
      OnClick = bAddClick
    end
    object txtAddIPAddress: TEdit
      Left = 90
      Top = 170
      Width = 145
      Height = 21
      TabOrder = 3
    end
    object txtAddSysName: TEdit
      Left = 240
      Top = 170
      Width = 158
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 4
    end
  end
  object GroupBox2: TGroupBox
    Left = 415
    Top = 34
    Width = 207
    Height = 201
    Anchors = [akTop, akRight]
    Caption = 'Settings'
    TabOrder = 1
    object Label2: TLabel
      Left = 10
      Top = 34
      Width = 72
      Height = 13
      Caption = 'SNMP Version:'
    end
    object cboVersion: TComboBox
      Left = 132
      Top = 30
      Width = 63
      Height = 21
      TabOrder = 0
      Text = 'v2c'
      OnChange = cboVersionChange
      Items.Strings = (
        'v1'
        'v2c'
        'v3')
    end
    object gAuthParams: TGroupBox
      Left = 10
      Top = 66
      Width = 189
      Height = 121
      Caption = 'SNMPv3 Authentication'
      Enabled = False
      TabOrder = 1
      object Label3: TLabel
        Left = 18
        Top = 34
        Width = 25
        Height = 13
        Caption = 'User:'
      end
      object Label4: TLabel
        Left = 18
        Top = 60
        Width = 51
        Height = 13
        Caption = 'Auth Pass:'
      end
      object Label5: TLabel
        Left = 18
        Top = 86
        Width = 64
        Height = 13
        Caption = 'Privacy Pass:'
      end
      object txtUser: TEdit
        Left = 88
        Top = 28
        Width = 91
        Height = 21
        TabOrder = 0
        Text = 'myuser'
      end
      object txtAPassword: TEdit
        Left = 88
        Top = 54
        Width = 91
        Height = 21
        TabOrder = 1
        Text = 'my_password'
      end
      object txtEPassword: TEdit
        Left = 88
        Top = 80
        Width = 91
        Height = 21
        TabOrder = 2
      end
    end
  end
  object GroupBox3: TGroupBox
    Left = 2
    Top = 238
    Width = 622
    Height = 211
    Anchors = [akLeft, akTop, akRight]
    Caption = 'System Values'
    TabOrder = 2
    DesignSize = (
      622
      211)
    object lvwResults: TListView
      Left = 6
      Top = 18
      Width = 608
      Height = 187
      Anchors = [akLeft, akTop, akRight]
      Columns = <
        item
          Caption = 'Object ID'
          Width = 150
        end
        item
          Caption = 'Name'
          Width = 150
        end
        item
          Caption = 'Value'
          Width = 304
        end>
      TabOrder = 0
      ViewStyle = vsReport
    end
  end
  object GroupBox4: TGroupBox
    Left = 2
    Top = 454
    Width = 622
    Height = 257
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Events'
    TabOrder = 3
    DesignSize = (
      622
      257)
    object lstEvents: TListBox
      Left = 6
      Top = 18
      Width = 610
      Height = 233
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 13
      TabOrder = 0
    end
  end
  object SNMPMgr1: TipnSNMPMgr
    Community = 'public'
    OnBadPacket = SNMPMgr1BadPacket
    OnError = SNMPMgr1Error
    OnPacketTrace = SNMPMgr1PacketTrace
    OnReport = SNMPMgr1Report
    OnResponse = SNMPMgr1Response
    Left = 24
    Top = 88
  end
end


