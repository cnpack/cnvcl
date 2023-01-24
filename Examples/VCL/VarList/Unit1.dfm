object Form1: TForm1
  Left = 282
  Top = 164
  BorderStyle = bsDialog
  Caption = 'CnVarList Test'
  ClientHeight = 424
  ClientWidth = 501
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 232
    Top = 16
    Width = 137
    Height = 25
    Caption = 'Restore Vars From String'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 0
    Top = 64
    Width = 501
    Height = 360
    Align = alBottom
    TabOrder = 1
  end
  object Button2: TButton
    Left = 16
    Top = 16
    Width = 145
    Height = 25
    Caption = 'Add Vars and ToString'
    TabOrder = 2
    OnClick = Button2Click
  end
end
