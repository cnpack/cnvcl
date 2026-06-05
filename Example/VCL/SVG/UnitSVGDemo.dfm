object FormSVGDemo: TFormSVGDemo
  Left = 257
  Top = 122
  Width = 618
  Height = 578
  Caption = 'SVG ªÊ÷∆'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object imgResult: TImage
    Left = 12
    Top = 84
    Width = 586
    Height = 450
    Anchors = [akLeft, akTop, akRight, akBottom]
    Stretch = True
  end
  object edtFileName: TEdit
    Left = 12
    Top = 12
    Width = 500
    Height = 21
    TabOrder = 0
  end
  object btnBrowse: TButton
    Left = 518
    Top = 10
    Width = 80
    Height = 25
    Caption = '‰Ø¿¿...'
    TabOrder = 1
    OnClick = btnBrowseClick
  end
  object cmbSize: TComboBox
    Left = 12
    Top = 48
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
    Items.Strings = (
      '64x64'
      '128x128'
      '256x256'
      '512x512')
  end
  object btnDraw: TButton
    Left = 166
    Top = 46
    Width = 100
    Height = 25
    Caption = 'ªÊ÷∆'
    TabOrder = 3
    OnClick = btnDrawClick
  end
  object dlgOpen: TOpenDialog
    Left = 280
    Top = 46
  end
end
