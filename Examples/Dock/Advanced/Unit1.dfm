object Form1: TForm1
  Left = 218
  Top = 258
  Width = 159
  Height = 170
  BorderStyle = bsSizeToolWin
  Caption = 'Delphi·ç¸ñ'
  Color = clBtnFace
  DockSite = True
  DragKind = dkDock
  DragMode = dmAutomatic
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Visible = True
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 151
    Height = 143
    Align = alClient
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
  end
  object CnDockClient1: TCnDockClient
    OnFormShow = CnDockClient1FormShow
    OnFormHide = CnDockClient1FormHide
    LRDockWidth = 100
    TBDockHeight = 100
    NCPopupMenu = MainForm.PopupMenu2
    DirectDrag = False
    ShowHint = True
    EnableCloseBtn = True
    Left = 48
    Top = 24
  end
end
