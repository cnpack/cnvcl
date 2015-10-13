object FormSpin: TFormSpin
  Left = 192
  Top = 107
  Width = 928
  Height = 455
  Caption = 'CnSpinEdit Test'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnCreate: TButton
    Left = 32
    Top = 24
    Width = 129
    Height = 25
    Caption = 'Create a CnSpinEdit'
    TabOrder = 0
    OnClick = btnCreateClick
  end
  object SpinEdit1: TSpinEdit
    Left = 240
    Top = 24
    Width = 121
    Height = 25
    MaxValue = 0
    MinValue = 0
    TabOrder = 1
    Value = 0
  end
end
