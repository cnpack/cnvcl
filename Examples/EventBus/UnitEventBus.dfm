object EventBusForm: TEventBusForm
  Left = 392
  Top = 226
  BorderStyle = bsDialog
  Caption = 'Event Bus Demo'
  ClientHeight = 217
  ClientWidth = 221
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object btnRegister: TButton
    Left = 32
    Top = 40
    Width = 161
    Height = 25
    Caption = 'Register a Receiver'
    TabOrder = 0
    OnClick = btnRegisterClick
  end
  object btnUnRegister: TButton
    Left = 32
    Top = 96
    Width = 161
    Height = 25
    Caption = 'UnRegister this Receiver'
    TabOrder = 1
    OnClick = btnUnRegisterClick
  end
  object btnPost: TButton
    Left = 32
    Top = 152
    Width = 161
    Height = 25
    Caption = 'Post a Event'
    TabOrder = 2
    OnClick = btnPostClick
  end
end
