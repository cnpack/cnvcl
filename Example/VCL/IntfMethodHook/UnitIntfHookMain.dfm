object IntfHookForm: TIntfHookForm
  Left = 200
  Top = 120
  BorderStyle = bsDialog
  Caption = 'CnIntfHook Example - Interface Method Hook Demo'
  ClientHeight = 522
  ClientWidth = 724
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 705
    Height = 497
    ActivePage = ts1
    TabOrder = 0
    object ts1: TTabSheet
      Caption = 'IntfHook Class'
      object mmoLog: TMemo
        Left = 8
        Top = 8
        Width = 676
        Height = 380
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
      object btnHookByIndex: TButton
        Left = 8
        Top = 400
        Width = 130
        Height = 25
        Caption = 'Hook by Index'
        TabOrder = 1
        OnClick = btnHookByIndexClick
      end
      object btnUnhookByIndex: TButton
        Left = 148
        Top = 400
        Width = 130
        Height = 25
        Caption = 'Unhook by Index'
        TabOrder = 2
        OnClick = btnUnhookByIndexClick
      end
      object btnHookUnhookToggle: TButton
        Left = 288
        Top = 400
        Width = 130
        Height = 25
        Caption = 'Toggle HookAdd'
        TabOrder = 3
        OnClick = btnHookUnhookToggleClick
      end
      object btnHookByName: TButton
        Left = 428
        Top = 400
        Width = 120
        Height = 25
        Caption = 'Hook by Name (RTTI)'
        TabOrder = 4
        OnClick = btnHookByNameClick
      end
      object btnUnhookByName: TButton
        Left = 556
        Top = 400
        Width = 128
        Height = 25
        Caption = 'Unhook by Name'
        TabOrder = 5
        OnClick = btnUnhookByNameClick
      end
      object btnClearLog: TButton
        Left = 288
        Top = 436
        Width = 130
        Height = 25
        Caption = 'Clear Log'
        TabOrder = 6
        OnClick = btnClearLogClick
      end
      object btnCallGetName: TButton
        Left = 148
        Top = 436
        Width = 130
        Height = 25
        Caption = 'Call GetName/SetValue'
        TabOrder = 7
        OnClick = btnCallGetNameClick
      end
      object btnCallAdd: TButton
        Left = 8
        Top = 436
        Width = 130
        Height = 25
        Caption = 'Call Add(10, 20)'
        TabOrder = 8
        OnClick = btnCallAddClick
      end
      object btnHookByVTable: TButton
        Left = 428
        Top = 436
        Width = 256
        Height = 25
        Caption = 'CreateAtVirtualTable Hook (Toggle)'
        TabOrder = 9
        OnClick = btnHookByVTableClick
      end
    end
    object ts2: TTabSheet
      Caption = 'Manually Test'
      ImageIndex = 1
      object btnHook: TButton
        Left = 40
        Top = 40
        Width = 75
        Height = 25
        Caption = 'Hook'
        TabOrder = 0
        OnClick = btnHookClick
      end
      object btnUnhook: TButton
        Left = 176
        Top = 40
        Width = 75
        Height = 25
        Caption = 'Unhook'
        TabOrder = 1
        OnClick = btnUnhookClick
      end
      object btnCall1: TButton
        Left = 312
        Top = 40
        Width = 97
        Height = 25
        Caption = 'Call Intf Method 1'
        TabOrder = 2
        OnClick = btnCall1Click
      end
      object btnCall2: TButton
        Left = 448
        Top = 40
        Width = 97
        Height = 25
        Caption = 'Call Intf Method 2'
        TabOrder = 3
        OnClick = btnCall2Click
      end
    end
  end
end
