object FormMibbrowser: TFormMibbrowser
  Left = 192
  Top = 107
  Width = 650
  Height = 638
  Caption = 'MibBrowser Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 625
    Height = 39
    Caption = 
      'This demo shows how to use the MibBrowser component.  Click on t' +
      'he "Load Mib" button to load a set of sample MIBs.  You can then' +
      ' browse through the tree on the left and view the details below.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object Label2: TLabel
    Left = 8
    Top = 72
    Width = 54
    Height = 13
    Caption = 'Mib Nodes:'
  end
  object Label3: TLabel
    Left = 344
    Top = 72
    Width = 112
    Height = 13
    Caption = 'Select Node Properties:'
  end
  object Label4: TLabel
    Left = 344
    Top = 360
    Width = 168
    Height = 13
    Caption = 'Selected Node Textual Description:'
  end
  object Label5: TLabel
    Left = 8
    Top = 416
    Width = 59
    Height = 13
    Caption = 'Trap Nodes:'
  end
  object btnLoadMibs: TButton
    Left = 8
    Top = 40
    Width = 75
    Height = 25
    Caption = '&Load Mib'
    TabOrder = 0
    OnClick = btnLoadMibsClick
  end
  object tvwMibNodes: TTreeView
    Left = 8
    Top = 88
    Width = 321
    Height = 313
    Indent = 19
    TabOrder = 1
    OnClick = tvwMibNodesClick
  end
  object lvwProperties: TListView
    Left = 344
    Top = 88
    Width = 289
    Height = 249
    Columns = <
      item
        Caption = 'Field'
        Width = 100
      end
      item
        Caption = 'Value'
        Width = 175
      end>
    TabOrder = 2
    ViewStyle = vsReport
  end
  object txtDescription: TMemo
    Left = 344
    Top = 376
    Width = 289
    Height = 225
    Lines.Strings = (
      'txtDescription')
    TabOrder = 3
  end
  object tvwTrapNodes: TTreeView
    Left = 8
    Top = 432
    Width = 321
    Height = 169
    Indent = 19
    TabOrder = 4
    OnClick = tvwTrapNodesClick
  end
  object MibBrowser1: TipnMibBrowser
    OnMibNode = MibBrowser1MibNode
    OnTrapNode = MibBrowser1TrapNode
    Left = 152
    Top = 40
  end
  object OpenDialog1: TOpenDialog
    Left = 120
    Top = 40
  end
end


