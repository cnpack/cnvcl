object FormIDCard: TFormIDCard
  Left = 484
  Top = 286
  BorderStyle = bsDialog
  Caption = 'Check Chinese ID Card Number'
  ClientHeight = 182
  ClientWidth = 264
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblIDCard: TLabel
    Left = 32
    Top = 32
    Width = 120
    Height = 13
    Caption = 'Chinese ID Card Number:'
  end
  object edtIDCard: TEdit
    Left = 32
    Top = 64
    Width = 193
    Height = 21
    TabOrder = 0
    Text = '36024719850847234X'
  end
  object btnCheck: TButton
    Left = 32
    Top = 112
    Width = 193
    Height = 25
    Caption = 'Check'
    TabOrder = 1
    OnClick = btnCheckClick
  end
end
