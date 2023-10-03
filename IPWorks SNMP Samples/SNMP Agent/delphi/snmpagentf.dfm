object FormSnmpagent: TFormSnmpagent
  Left = 186
  Top = 108
  Caption = 'SNMP Agent'
  ClientHeight = 615
  ClientWidth = 468
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    468
    615)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 4
    Top = 6
    Width = 445
    Height = 39
    Caption = 
      'This is a demo of a simple SNMP Agent, able to respond to reques' +
      'ts and send traps when appropriate.  The SNMP component implemen' +
      'ts SNMP V1, V2c, and V3.  Edit the appropriate object values and' +
      ' click "Start Listening" to begin.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object TabControl1: TTabControl
    Left = 2
    Top = 64
    Width = 474
    Height = 245
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Tabs.Strings = (
      'Object ID'#39's'
      'Send Trap'
      'User Mgmt')
    TabIndex = 0
    OnChange = TabControl1Change
    DesignSize = (
      474
      245)
    object pnlUsers: TPanel
      Left = -2
      Top = 18
      Width = 475
      Height = 227
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      DesignSize = (
        475
        227)
      object Label15: TLabel
        Left = 10
        Top = 6
        Width = 217
        Height = 13
        Caption = 'Anonymous (unauthenticated) User Privileges:'
      end
      object cbAnonGet: TCheckBox
        Left = 240
        Top = 6
        Width = 47
        Height = 21
        Caption = 'Get'
        Checked = True
        State = cbChecked
        TabOrder = 0
      end
      object cbAnonSet: TCheckBox
        Left = 298
        Top = 6
        Width = 61
        Height = 23
        Caption = 'Set'
        TabOrder = 1
      end
      object GroupBox1: TGroupBox
        Left = 4
        Top = 28
        Width = 461
        Height = 179
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Authenticated User Privileges (SNMPv3 only)'
        TabOrder = 2
        DesignSize = (
          461
          179)
        object lvwUsers: TListView
          Left = 8
          Top = 18
          Width = 369
          Height = 127
          Anchors = [akLeft, akTop, akRight]
          Columns = <
            item
              Caption = 'User'
              Width = 105
            end
            item
              Caption = 'Auth Pass'
              Width = 100
            end
            item
              Caption = 'Enc. Pass'
              Width = 100
            end
            item
              Caption = 'Get'
              Width = 30
            end
            item
              Caption = 'Set'
              Width = 30
            end>
          TabOrder = 0
          ViewStyle = vsReport
        end
        object tbNewUser: TEdit
          Left = 10
          Top = 150
          Width = 99
          Height = 21
          TabOrder = 1
          Text = 'newuser'
        end
        object tbNewAuthPass: TEdit
          Left = 112
          Top = 150
          Width = 99
          Height = 21
          TabOrder = 2
        end
        object tbNewEncPass: TEdit
          Left = 214
          Top = 150
          Width = 99
          Height = 21
          TabOrder = 3
        end
        object cbNewGet: TCheckBox
          Left = 324
          Top = 152
          Width = 19
          Height = 21
          Checked = True
          State = cbChecked
          TabOrder = 4
        end
        object cbNewSet: TCheckBox
          Left = 350
          Top = 152
          Width = 19
          Height = 21
          TabOrder = 5
        end
        object bAdd: TButton
          Left = 380
          Top = 150
          Width = 69
          Height = 23
          Anchors = [akTop, akRight]
          Caption = 'Add'
          TabOrder = 6
          OnClick = bAddClick
        end
        object Remove: TButton
          Left = 380
          Top = 122
          Width = 71
          Height = 23
          Anchors = [akTop, akRight]
          Caption = 'Remove'
          TabOrder = 7
          OnClick = RemoveClick
        end
      end
    end
    object pnlTraps: TPanel
      Left = -2
      Top = 18
      Width = 475
      Height = 227
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      OnClick = pnlTrapsClick
      DesignSize = (
        475
        227)
      object Label10: TLabel
        Left = 12
        Top = 24
        Width = 47
        Height = 13
        Caption = 'Trap OID:'
      end
      object Label11: TLabel
        Left = 12
        Top = 48
        Width = 65
        Height = 13
        Caption = 'Remote Host:'
      end
      object Label12: TLabel
        Left = 12
        Top = 78
        Width = 47
        Height = 13
        Caption = 'Trap OID:'
      end
      object cbTrapOID: TComboBox
        Left = 68
        Top = 16
        Width = 145
        Height = 21
        TabOrder = 0
        Text = 'coldStart'
        Items.Strings = (
          'coldStart'
          'warmStart'
          'linkdown'
          'linkup'
          'authenticationFailure')
      end
      object txtRemHost: TEdit
        Left = 84
        Top = 48
        Width = 129
        Height = 21
        TabOrder = 1
        Text = '255.255.255.255'
      end
      object rbv1: TRadioButton
        Left = 68
        Top = 80
        Width = 49
        Height = 9
        Caption = 'v1'
        Checked = True
        TabOrder = 2
        TabStop = True
      end
      object rbv2c: TRadioButton
        Left = 124
        Top = 80
        Width = 49
        Height = 9
        Caption = 'v2c'
        TabOrder = 3
      end
      object rbv3: TRadioButton
        Left = 180
        Top = 80
        Width = 49
        Height = 9
        Caption = 'v3'
        TabOrder = 4
      end
      object bSendTrap: TButton
        Left = 174
        Top = 152
        Width = 97
        Height = 25
        Caption = 'Send Trap'
        TabOrder = 5
        OnClick = bSendTrapClick
      end
      object gbAuthPriv: TGroupBox
        Left = 224
        Top = 12
        Width = 231
        Height = 117
        Anchors = [akLeft, akTop, akRight]
        Caption = 'gbAuthPriv'
        TabOrder = 6
        DesignSize = (
          231
          117)
        object Label13: TLabel
          Left = 16
          Top = 28
          Width = 25
          Height = 13
          Caption = 'User:'
        end
        object Label14: TLabel
          Left = 14
          Top = 56
          Width = 51
          Height = 13
          Caption = 'Auth Pass:'
        end
        object Label9: TLabel
          Left = 14
          Top = 84
          Width = 64
          Height = 13
          Caption = 'Privacy Pass:'
        end
        object txtTrapUser: TEdit
          Left = 98
          Top = 26
          Width = 129
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          Text = 'myuser'
        end
        object txtTrapAPassword: TEdit
          Left = 98
          Top = 54
          Width = 129
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 1
          Text = 'my_password'
        end
        object txtTrapEPassword: TEdit
          Left = 98
          Top = 82
          Width = 129
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 2
          Text = 'my_password'
        end
      end
    end
    object pnlObjectIds: TPanel
      Left = 0
      Top = 18
      Width = 473
      Height = 227
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      DesignSize = (
        473
        227)
      object Label2: TLabel
        Left = 12
        Top = 14
        Width = 48
        Height = 13
        Caption = 'Engine Id:'
      end
      object Label3: TLabel
        Left = 12
        Top = 38
        Width = 46
        Height = 13
        Caption = 'sysDescr:'
      end
      object Label4: TLabel
        Left = 12
        Top = 62
        Width = 60
        Height = 13
        Caption = 'sysObjectID:'
      end
      object Label5: TLabel
        Left = 12
        Top = 86
        Width = 55
        Height = 13
        Caption = 'sysUpTime:'
      end
      object Label6: TLabel
        Left = 12
        Top = 110
        Width = 55
        Height = 13
        Caption = 'sysContact:'
      end
      object Label7: TLabel
        Left = 12
        Top = 134
        Width = 46
        Height = 13
        Caption = 'sysName:'
      end
      object Label8: TLabel
        Left = 12
        Top = 158
        Width = 59
        Height = 13
        Caption = 'sysLocation:'
      end
      object txtEngineID: TEdit
        Left = 76
        Top = 12
        Width = 391
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = 'MyEngine'
        OnChange = txtEngineIDChange
      end
      object txtOIDVal0: TEdit
        Left = 76
        Top = 36
        Width = 391
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        Text = 'My System'
      end
      object txtOIDVal1: TEdit
        Left = 76
        Top = 60
        Width = 391
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
        Text = '1.3.6.1.4.1.127.0.0.1'
      end
      object txtOIDVal2: TEdit
        Left = 76
        Top = 84
        Width = 391
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 3
        Text = '0'
      end
      object txtOIDVal3: TEdit
        Left = 76
        Top = 108
        Width = 391
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 4
        Text = '/n software: sales@nsoftware.com'
      end
      object txtOIDVal4: TEdit
        Left = 76
        Top = 132
        Width = 391
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 5
        Text = 'Localhost'
      end
      object txtOIDVal5: TEdit
        Left = 76
        Top = 156
        Width = 391
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 6
        Text = '(unknown)'
      end
    end
  end
  object bListen: TButton
    Left = 178
    Top = 320
    Width = 97
    Height = 25
    Caption = 'Start &Listening'
    TabOrder = 1
    OnClick = bListenClick
  end
  object GroupBox2: TGroupBox
    Left = 2
    Top = 364
    Width = 472
    Height = 252
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Event Log'
    TabOrder = 2
    DesignSize = (
      472
      252)
    object lstEvents: TListBox
      Left = 6
      Top = 18
      Width = 460
      Height = 228
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 13
      TabOrder = 0
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 350
    Top = 38
  end
  object SNMPAgent1: TipnSNMPAgent
    OnBadPacket = SNMPAgent1BadPacket
    OnDiscoveryRequest = SNMPAgent1DiscoveryRequest
    OnError = SNMPAgent1Error
    OnGetBulkRequest = SNMPAgent1GetBulkRequest
    OnGetNextRequest = SNMPAgent1GetNextRequest
    OnGetRequest = SNMPAgent1GetRequest
    OnGetUserPassword = SNMPAgent1GetUserPassword
    OnPacketTrace = SNMPAgent1PacketTrace
    OnReport = SNMPAgent1Report
    OnSetRequest = SNMPAgent1SetRequest
    Left = 304
    Top = 312
    RegHnd = {}
  end
end


