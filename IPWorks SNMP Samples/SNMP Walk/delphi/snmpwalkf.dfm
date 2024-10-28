object FormSnmpwalk: TFormSnmpwalk
  Left = 0
  Top = 0
  Caption = 'SNMPWalk Demo'
  ClientHeight = 525
  ClientWidth = 780
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 697
    Height = 13
    Caption = 
      'This demo shows how to perform an SNMP Walk using SnmpMgr.  By d' +
      'efault, the demo will walk the "system" table, starting at oid 1' +
      '.3.6.1.2.1.1. '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 40
    Width = 761
    Height = 105
    Caption = 'SNMP Settings'
    TabOrder = 0
    object Label2: TLabel
      Left = 17
      Top = 24
      Width = 63
      Height = 13
      Caption = 'SNMP Agent:'
    end
    object Label3: TLabel
      Left = 16
      Top = 48
      Width = 69
      Height = 13
      Caption = 'SNMP Version:'
    end
    object Label4: TLabel
      Left = 258
      Top = 24
      Width = 26
      Height = 13
      Caption = 'User:'
    end
    object Label5: TLabel
      Left = 258
      Top = 48
      Width = 52
      Height = 13
      Caption = 'Auth Pass:'
    end
    object Label6: TLabel
      Left = 258
      Top = 75
      Width = 46
      Height = 13
      Caption = 'Enc Pass:'
    end
    object Label7: TLabel
      Left = 538
      Top = 24
      Width = 64
      Height = 13
      Caption = 'Starting OID:'
    end
    object txtAgent: TEdit
      Left = 105
      Top = 21
      Width = 121
      Height = 21
      TabOrder = 0
    end
    object txtUser: TEdit
      Left = 328
      Top = 21
      Width = 121
      Height = 21
      Enabled = False
      TabOrder = 2
    end
    object txtAuthPass: TEdit
      Left = 328
      Top = 48
      Width = 121
      Height = 21
      Enabled = False
      PasswordChar = '*'
      TabOrder = 3
    end
    object txtEncPass: TEdit
      Left = 328
      Top = 75
      Width = 121
      Height = 21
      Enabled = False
      PasswordChar = '*'
      TabOrder = 4
    end
    object cboAuthAlg: TComboBox
      Left = 455
      Top = 48
      Width = 57
      Height = 21
      Enabled = False
      TabOrder = 5
      Text = 'cboAuthAlg'
      Items.Strings = (
        'MD5'
        'SHA1')
    end
    object cboEncAlg: TComboBox
      Left = 455
      Top = 75
      Width = 57
      Height = 21
      Enabled = False
      TabOrder = 6
      Text = 'cboEncAlg'
      Items.Strings = (
        'DES'
        'AES'
        '3DES')
    end
    object txtOID: TEdit
      Left = 608
      Top = 21
      Width = 121
      Height = 21
      TabOrder = 7
      Text = '1.3.6.1.2.1.1'
    end
    object btnWalk: TButton
      Left = 608
      Top = 48
      Width = 75
      Height = 25
      Caption = '&Walk'
      TabOrder = 8
      OnClick = btnWalkClick
    end
    object cboSNMPVersion: TComboBox
      Left = 105
      Top = 48
      Width = 121
      Height = 21
      TabOrder = 1
      Text = 'cboSNMPVersion'
      OnChange = cboSNMPVersionChange
      Items.Strings = (
        'SNMPv1'
        'SNMPv2'
        'SNMPv3')
    end
  end
  object lvwObjects: TListView
    Left = 8
    Top = 151
    Width = 761
    Height = 361
    Columns = <
      item
        Caption = 'Object Id'
        Width = 200
      end
      item
        Caption = 'Type'
        Width = 150
      end
      item
        Caption = 'Value'
        Width = 250
      end>
    TabOrder = 1
    ViewStyle = vsReport
  end
  object SNMPMgr1: TipnSNMPMgr
    Community = 'public'
    Left = 544
    Top = 88
  end
end


