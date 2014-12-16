object Form2: TForm2
  Left = 319
  Top = 200
  BorderStyle = bsDialog
  Caption = 'Dialog'
  ClientHeight = 169
  ClientWidth = 395
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object edt1: TEdit
    Left = 48
    Top = 40
    Width = 305
    Height = 21
    TabOrder = 0
    Text = 'edt1'
  end
  object btn1: TCnButton
    Left = 88
    Top = 96
    Width = 75
    Height = 25
    Caption = '&OK'
    Color = clBtnFace
    Default = True
    DownBold = False
    ModalResult = 1
    ParentColor = False
    TabOrder = 1
    TabStop = True
  end
  object btn2: TCnButton
    Left = 224
    Top = 96
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    Color = clBtnFace
    DownBold = False
    ModalResult = 2
    ParentColor = False
    TabOrder = 2
    TabStop = True
  end
end
