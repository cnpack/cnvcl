object FormMDNS: TFormMDNS
  Left = 443
  Top = 302
  Width = 624
  Height = 432
  Caption = 'mDNS Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object BtnStart: TButton
    Left = 16
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 0
    OnClick = BtnStartClick
  end
  object BtnBrowse: TButton
    Left = 104
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Browse'
    TabOrder = 1
    OnClick = BtnBrowseClick
  end
  object BtnRegister: TButton
    Left = 192
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Register'
    TabOrder = 2
    OnClick = BtnRegisterClick
  end
  object EditType: TEdit
    Left = 16
    Top = 56
    Width = 250
    Height = 21
    TabOrder = 3
    Text = '_http._tcp.local'
  end
  object EditInstance: TEdit
    Left = 16
    Top = 88
    Width = 250
    Height = 21
    TabOrder = 4
    Text = 'Test Service._http._tcp.local'
  end
  object EditPort: TEdit
    Left = 16
    Top = 120
    Width = 80
    Height = 21
    TabOrder = 5
    Text = '8080'
  end
  object MemoLog: TMemo
    Left = 16
    Top = 160
    Width = 560
    Height = 220
    TabOrder = 6
  end
end
