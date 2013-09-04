object Form1: TForm1
  Left = 447
  Top = 264
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'CnHardwareBreakPoints Demo'
  ClientHeight = 222
  ClientWidth = 262
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 94
    Top = 15
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 94
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 94
    Top = 65
    Width = 75
    Height = 25
    Caption = 'Button3'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 94
    Top = 90
    Width = 75
    Height = 25
    Caption = 'Button4'
    TabOrder = 3
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 24
    Top = 135
    Width = 225
    Height = 25
    Caption = 'Set HardwareBreakpoint at Buton1-4'
    TabOrder = 4
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 24
    Top = 166
    Width = 225
    Height = 25
    Caption = 'Unset HardwareBreakpoint '
    TabOrder = 5
    OnClick = Button6Click
  end
end
