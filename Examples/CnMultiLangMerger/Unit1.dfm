object CnMultiLangMergeFrm: TCnMultiLangMergeFrm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'CnPack Multi-Language Items Merger'
  ClientHeight = 486
  ClientWidth = 583
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    583
    486)
  PixelsPerInch = 96
  TextHeight = 13
  object lbl1: TLabel
    Left = 8
    Top = 8
    Width = 211
    Height = 39
    Caption = 
      'Modified Source Language Items:'#13#10'Please paste modified language ' +
      'items here. '#13#10'e.g. 1033'#39's English file content.'
  end
  object lbl2: TLabel
    Left = 295
    Top = 8
    Width = 198
    Height = 39
    Anchors = [akTop, akRight]
    Caption = 
      'Target Language Items to Update:'#13#10'Please paste target language i' +
      'tems here.'#13#10'e.g. 1031 or 1049 file content.'
  end
  object lbl3: TLabel
    Left = 8
    Top = 248
    Width = 562
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 
      'Merged Target Language Items. Contains updated target language i' +
      'tems with new Modified items but not translated.'
  end
  object lbl4: TLabel
    Left = 8
    Top = 454
    Width = 400
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 
      'Note: Please use Delphi 2009 or Later to Compile this Program fo' +
      'r Unicode Support.'
    ExplicitTop = 411
  end
  object mmo1: TMemo
    Left = 8
    Top = 53
    Width = 273
    Height = 184
    Anchors = [akLeft, akTop, akBottom]
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object mmo2: TMemo
    Left = 294
    Top = 53
    Width = 273
    Height = 184
    Anchors = [akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
    ExplicitLeft = 299
  end
  object mmo3: TMemo
    Left = 8
    Top = 267
    Width = 559
    Height = 177
    Anchors = [akLeft, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 2
    WordWrap = False
    ExplicitTop = 224
    ExplicitWidth = 561
  end
  object btnMerge: TButton
    Left = 418
    Top = 450
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Merge'
    TabOrder = 3
    OnClick = btnMergeClick
    ExplicitLeft = 420
    ExplicitTop = 407
  end
  object btnCopy: TButton
    Left = 499
    Top = 450
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Copy'
    TabOrder = 4
    OnClick = btnCopyClick
    ExplicitLeft = 501
    ExplicitTop = 407
  end
end
