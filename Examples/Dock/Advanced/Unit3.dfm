object Form3: TForm3
  Left = 233
  Top = 290
  Width = 148
  Height = 172
  BorderStyle = bsSizeToolWin
  Caption = 'VID·ç¸ñ'
  Color = clBtnFace
  DockSite = True
  DragKind = dkDock
  DragMode = dmAutomatic
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Visible = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 140
    Height = 145
    Align = alClient
    BevelInner = bvRaised
    BevelOuter = bvLowered
    Caption = 'Panel1'
    TabOrder = 0
    object Memo1: TMemo
      Left = 2
      Top = 2
      Width = 136
      Height = 141
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
    DirectDrag = False
    ShowHint = True
    DockStyle = MainForm.CnVIDDockStyle1
    Left = 64
    Top = 32
  end
end
