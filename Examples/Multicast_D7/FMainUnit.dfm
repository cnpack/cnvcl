object frmMain: TfrmMain
  Left = 367
  Top = 282
  BorderStyle = bsDialog
  Caption = 'Multicast Test'
  ClientHeight = 327
  ClientWidth = 301
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object EHF: TButton
    Left = 8
    Top = 8
    Width = 181
    Height = 25
    Caption = 'Event Handler Firer'
    TabOrder = 0
    OnClick = EHFClick
  end
  object AEH1: TButton
    Left = 8
    Top = 39
    Width = 137
    Height = 25
    Caption = 'Add Event Handler 1'
    TabOrder = 1
    OnClick = AddEventHandlers
  end
  object REH1: TButton
    Left = 156
    Top = 39
    Width = 137
    Height = 25
    Caption = 'Remove Event Handler 1'
    TabOrder = 2
    OnClick = RemoveEventHandlers
  end
  object AEH2: TButton
    Tag = 1
    Left = 8
    Top = 70
    Width = 137
    Height = 25
    Caption = 'Add Event Handler 2'
    TabOrder = 3
    OnClick = AddEventHandlers
  end
  object REH2: TButton
    Tag = 1
    Left = 156
    Top = 70
    Width = 137
    Height = 25
    Caption = 'Remove Event Handler 2'
    TabOrder = 4
    OnClick = RemoveEventHandlers
  end
  object AEH3: TButton
    Tag = 2
    Left = 8
    Top = 101
    Width = 137
    Height = 25
    Caption = 'Add Event Handler 3'
    TabOrder = 5
    OnClick = AddEventHandlers
  end
  object REH3: TButton
    Tag = 2
    Left = 156
    Top = 101
    Width = 137
    Height = 25
    Caption = 'Remove Event Handler 3'
    TabOrder = 6
    OnClick = RemoveEventHandlers
  end
  object AEH4: TButton
    Tag = 3
    Left = 8
    Top = 132
    Width = 137
    Height = 25
    Caption = 'Add Event Handler 4'
    TabOrder = 7
    OnClick = AddEventHandlers
  end
  object REH4: TButton
    Tag = 3
    Left = 156
    Top = 132
    Width = 137
    Height = 25
    Caption = 'Remove Event Handler 4'
    TabOrder = 8
    OnClick = RemoveEventHandlers
  end
  object CAEH: TButton
    Left = 8
    Top = 163
    Width = 285
    Height = 25
    Caption = 'Clear All Event Handlers'
    TabOrder = 9
    OnClick = CAEHClick
  end
  object mmoLog: TMemo
    Left = 8
    Top = 194
    Width = 285
    Height = 123
    ScrollBars = ssVertical
    TabOrder = 10
  end
  object chbAutoFire: TCheckBox
    Left = 195
    Top = 12
    Width = 98
    Height = 17
    Caption = 'Auto fire events'
    Checked = True
    State = cbChecked
    TabOrder = 11
    OnClick = chbAutoFireClick
  end
end
