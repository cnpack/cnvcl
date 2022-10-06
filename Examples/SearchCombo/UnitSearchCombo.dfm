object FormSearchCombo: TFormSearchCombo
  Left = 192
  Top = 107
  Width = 979
  Height = 563
  Caption = 'Test for Search Combo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object grpBox: TGroupBox
    Left = 16
    Top = 16
    Width = 257
    Height = 273
    Caption = 'Test Box'
    TabOrder = 0
    object lblDrawIndent: TLabel
      Left = 24
      Top = 232
      Width = 61
      Height = 13
      Caption = 'Draw Indent:'
    end
    object lblBoxSearch: TLabel
      Left = 24
      Top = 72
      Width = 37
      Height = 13
      Caption = 'Search:'
    end
    object btnShowBox: TButton
      Left = 24
      Top = 24
      Width = 89
      Height = 25
      Caption = 'Show/Hide Box'
      TabOrder = 0
      OnClick = btnShowBoxClick
    end
    object edtBox: TEdit
      Left = 80
      Top = 72
      Width = 153
      Height = 21
      TabOrder = 1
      OnChange = edtBoxChange
    end
    object chkBoxCase: TCheckBox
      Left = 144
      Top = 32
      Width = 97
      Height = 17
      Caption = 'Case Sensitive'
      TabOrder = 2
      OnClick = chkBoxCaseClick
    end
    object rgBoxMatchMode: TRadioGroup
      Left = 24
      Top = 112
      Width = 209
      Height = 105
      Caption = 'Box Match Mode'
      ItemIndex = 0
      Items.Strings = (
        'From Start'
        'Anywhere'
        'Fuzzy')
      TabOrder = 3
      OnClick = rgBoxMatchModeClick
    end
    object edtBoxIndent: TEdit
      Left = 96
      Top = 232
      Width = 121
      Height = 21
      TabOrder = 4
      Text = '0'
      OnChange = edtBoxIndentChange
    end
    object udBox: TUpDown
      Left = 217
      Top = 232
      Width = 15
      Height = 21
      Associate = edtBoxIndent
      Min = 0
      Position = 0
      TabOrder = 5
      Wrap = False
    end
  end
  object cbb1: TComboBox
    Left = 16
    Top = 312
    Width = 145
    Height = 21
    ItemHeight = 13
    TabOrder = 1
    Text = 'cbb1'
    OnChange = cbb1Change
    Items.Strings = (
      '1'
      '2'
      '3')
  end
  object cbb2: TComboBox
    Left = 16
    Top = 344
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
    OnChange = cbb1Change
    Items.Strings = (
      '1'
      '2'
      '3')
  end
  object grpTestCombo: TGroupBox
    Left = 288
    Top = 16
    Width = 353
    Height = 281
    Caption = 'Test Search Combo'
    TabOrder = 3
    object lblComboSearch: TLabel
      Left = 24
      Top = 72
      Width = 37
      Height = 13
      Caption = 'Search:'
    end
    object lblComboDrawIndent: TLabel
      Left = 24
      Top = 232
      Width = 61
      Height = 13
      Caption = 'Draw Indent:'
    end
    object chkComboCase: TCheckBox
      Left = 144
      Top = 32
      Width = 97
      Height = 17
      Caption = 'Case Sensitive'
      TabOrder = 0
      OnClick = chkComboCaseClick
    end
    object rgComboMatchMode: TRadioGroup
      Left = 24
      Top = 112
      Width = 209
      Height = 105
      Caption = 'Box Match Mode'
      ItemIndex = 0
      Items.Strings = (
        'From Start'
        'Anywhere'
        'Fuzzy')
      TabOrder = 1
      OnClick = rgComboMatchModeClick
    end
    object edtComboIndent: TEdit
      Left = 96
      Top = 232
      Width = 121
      Height = 21
      TabOrder = 2
      Text = '0'
      OnChange = edtComboIndentChange
    end
    object udCombo: TUpDown
      Left = 217
      Top = 232
      Width = 15
      Height = 21
      Associate = edtComboIndent
      Min = 0
      Position = 0
      TabOrder = 3
      Wrap = False
    end
    object btnCreateCombo: TButton
      Left = 24
      Top = 24
      Width = 89
      Height = 25
      Caption = 'Create Combo'
      TabOrder = 4
      OnClick = btnCreateComboClick
    end
  end
  object lst1: TListBox
    Left = 184
    Top = 312
    Width = 121
    Height = 97
    ItemHeight = 13
    Items.Strings = (
      '111'
      '222'
      '333'
      '444'
      '555')
    TabOrder = 4
    OnClick = lst1Click
  end
end
