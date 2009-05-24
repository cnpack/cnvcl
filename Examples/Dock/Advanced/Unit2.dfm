object Form2: TForm2
  Left = 311
  Top = 300
  Width = 137
  Height = 130
  BorderStyle = bsSizeToolWin
  Caption = 'VC++·ç¸ñ'
  Color = clBtnFace
  DockSite = True
  DragKind = dkDock
  DragMode = dmAutomatic
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'ËÎÌå'
  Font.Style = []
  OldCreateOrder = False
  Visible = True
  PixelsPerInch = 96
  TextHeight = 12
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 129
    Height = 103
    Align = alClient
    BevelInner = bvRaised
    BevelOuter = bvLowered
    Caption = 'Panel1'
    TabOrder = 0
    object Memo1: TMemo
      Left = 2
      Top = 2
      Width = 125
      Height = 99
      Align = alClient
      BorderStyle = bsNone
      Lines.Strings = (
        'Memo1')
      TabOrder = 0
    end
  end
  object CnDockClient1: TCnDockClient
    OnFormShow = CnDockClient1FormShow
    OnFormHide = CnDockClient1FormHide
    LRDockWidth = 100
    TBDockHeight = 100
    NCPopupMenu = MainForm.PopupMenu2
    DirectDrag = True
    ShowHint = True
    EnableCloseBtn = True
    EachOtherDock = False
    DockStyle = MainForm.CnVCDockStyle1
    Left = 48
    Top = 40
  end
end
