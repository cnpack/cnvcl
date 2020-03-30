object FormKDF: TFormKDF
  Left = 594
  Top = 190
  Width = 815
  Height = 675
  Caption = 'Key Derivation Function'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object grpKeyDerivation: TGroupBox
    Left = 24
    Top = 24
    Width = 729
    Height = 137
    Caption = 'Key Derivation'
    TabOrder = 0
    object lblPass: TLabel
      Left = 16
      Top = 32
      Width = 26
      Height = 13
      Caption = 'Pass:'
    end
    object lblSalt: TLabel
      Left = 192
      Top = 32
      Width = 49
      Height = 13
      Caption = 'Salt (Hex):'
    end
    object lblKeyHash: TLabel
      Left = 392
      Top = 32
      Width = 46
      Height = 13
      Caption = 'KeyHash:'
    end
    object lblNeedLength: TLabel
      Left = 528
      Top = 32
      Width = 68
      Height = 13
      Caption = 'Need Length: '
    end
    object lblBytes: TLabel
      Left = 664
      Top = 32
      Width = 29
      Height = 13
      Caption = 'Bytes.'
    end
    object edtPass: TEdit
      Left = 48
      Top = 28
      Width = 121
      Height = 21
      TabOrder = 0
      Text = '123456'
    end
    object edtSalt: TEdit
      Left = 248
      Top = 28
      Width = 121
      Height = 21
      TabOrder = 1
      Text = '123456'
    end
    object cbbLoadKeyHash: TComboBox
      Left = 448
      Top = 28
      Width = 49
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 2
      Items.Strings = (
        'MD5'
        'SHA256')
    end
    object edtNeedLength: TEdit
      Left = 608
      Top = 28
      Width = 49
      Height = 21
      TabOrder = 3
      Text = '32'
    end
    object btnGetKeyToHex: TButton
      Left = 16
      Top = 72
      Width = 97
      Height = 25
      Caption = 'Get Key To Hex'
      TabOrder = 4
      OnClick = btnGetKeyToHexClick
    end
    object edtGetKeyToHex: TEdit
      Left = 136
      Top = 72
      Width = 545
      Height = 21
      TabOrder = 5
    end
  end
end
