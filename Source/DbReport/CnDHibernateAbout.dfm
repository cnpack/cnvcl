object CnFormDHibernateAbout: TCnFormDHibernateAbout
  Left = 280
  Top = 326
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'About CnPack Delphi Hibernate'
  ClientHeight = 188
  ClientWidth = 235
  Color = clBtnFace
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = '宋体'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 12
  object Label1: TLabel
    Left = 34
    Top = 16
    Width = 175
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'CnPack Delphi Hibernate'
  end
  object lblVersion: TLabel
    Left = 34
    Top = 35
    Width = 175
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'Version: %d'
  end
  object Label3: TLabel
    Left = 34
    Top = 54
    Width = 175
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'Author: rarnu'
  end
  object Label8: TLabel
    Left = 34
    Top = 73
    Width = 175
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'CnPack 开发组'
  end
  object Bevel1: TBevel
    Left = 8
    Top = 104
    Width = 217
    Height = 9
    Shape = bsTopLine
  end
  object Label4: TLabel
    Left = 8
    Top = 120
    Width = 321
    Height = 13
    AutoSize = False
    Caption = '(C)Copyright 2001-2011 CnPack 开发组 '
  end
  object btnOK: TButton
    Left = 80
    Top = 147
    Width = 75
    Height = 25
    Cancel = True
    Caption = '确定'
    Default = True
    ModalResult = 2
    TabOrder = 0
  end
end
