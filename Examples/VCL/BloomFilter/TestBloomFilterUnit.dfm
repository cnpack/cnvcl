object BloomFilterForm: TBloomFilterForm
  Left = 192
  Top = 130
  Width = 928
  Height = 480
  Caption = 'String Bloom Filter Test'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object btnGenerate: TButton
    Left = 48
    Top = 48
    Width = 169
    Height = 25
    Caption = 'Generate 10000 string'
    TabOrder = 0
    OnClick = btnGenerateClick
  end
  object chkDuplicate: TCheckBox
    Left = 256
    Top = 48
    Width = 129
    Height = 17
    Caption = 'Check Duplicate'
    Checked = True
    State = cbChecked
    TabOrder = 1
  end
end
