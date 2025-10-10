{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2025 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��https://www.cnpack.org                                  }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnGraphRegister;
{* |<PRE>
================================================================================
* ������ƣ�����ؼ���
* ��Ԫ���ƣ�����ؼ���ע�ᵥԪ
* ��Ԫ���ߣ�CnPack������
* ��    ע��
* ����ƽ̨��PWin98SE + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2002.03.30 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  {$IFNDEF FPC}
  {$IFDEF COMPILER6_UP} DesignIntf, DesignEditors, {$ELSE} DsgnIntf, {$ENDIF}
  {$ENDIF}
  CnConsts, {$IFNDEF FPC} CnGraphics, CnImage, CnGraphPropEditors,
  CnWizardImage, CnShellCtrls, {$ENDIF}
  CnWaterImage, CnPanel, CnEdit, CnSpin, CnCheckTreeView,
  {$IFNDEF FPC} CnAOTreeView, CnAACtrls,
  CnAAFont, CnAAFontEditor, CnAAFontDialog, CnTabSet, CnButtonEdit,
  CnSkinMagic, CnButtons, CnHint, CnGauge, CnListBox, CnColorGrid,
  CnMonthCalendar, CnValidateImage, CnErrorProvider, CnLED, {$ENDIF} CnHexEditor;

procedure Register;
{* �ؼ�������༭�������Ա༭��ע�����}

implementation

procedure Register;
begin
  // RegisterComponents(SCnGraphicPalette, [TCnPaintBox]);
  // RegisterComponents(SCnGraphicPalette, [TCnImage]);
{$IFNDEF FPC}
  RegisterComponents(SCnGraphicPalette, [TCnButton, TCnBitBtn, TCnSpeedButton]);
{$ENDIF}
  RegisterComponents(SCnGraphicPalette, [TCnPanel, TCnEdit, TCnSpinEdit]);
{$IFNDEF FPC}
  RegisterComponents(SCnGraphicPalette, [TCnListBox]);
  RegisterComponents(SCnGraphicPalette, [TCnTabSet, TCnButtonEdit]);
{$ENDIF}
  RegisterComponents(SCnGraphicPalette, [TCnCheckTreeView]);
{$IFNDEF FPC}
  RegisterComponents(SCnGraphicPalette, [TCnAOTreeView]);
  RegisterComponents(SCnGraphicPalette, [TCnWizardImage]);
  RegisterComponents(SCnGraphicPalette, [TCnValidateImage]);
  RegisterComponents(SCnGraphicPalette, [TCnGauge]);
{$ENDIF}
  RegisterComponents(SCnGraphicPalette, [TCnWaterImage]);
  RegisterComponents(SCnGraphicPalette, [TCnHexEditor]);
{$IFNDEF FPC}
  RegisterComponents(SCnGraphicPalette, [TCnMonthCalendar]);
  RegisterComponents(SCnGraphicPalette, [TCnColorGrid]);
  RegisterComponents(SCnGraphicPalette, [TCnErrorProvider]);
  RegisterComponents(SCnGraphicPalette, [TCnLEDText]);
  RegisterComponents(SCnGraphicPalette, [TCnSkinMagic]);
  RegisterComponents(SCnGraphicPalette, [TCnHint, TCnHintWindow]);
  // RegisterComponents(SCnGraphicPalette, [TCnCheckGroupBox]);
  // RegisterPropertyEditor(TypeInfo(TCnBitmap), nil, '', TCnBitmapProperty);
  RegisterComponents(SCnGraphicPalette, [TCnShellTreeView, TCnShellListView,
    TCnShellChangeNotifier]);

  RegisterComponents(SCnGraphicPalette, [TCnAAFadeText, TCnAALabel, 
    TCnAALinkLabel, TCnAAText, TCnAAScrollText, TCnAAMarqueeText]);
  RegisterComponents(SCnGraphicPalette, [TCnAAFontDialog]);
  RegisterPropertyEditor(TypeInfo(TCnAAEffect), nil, '', TCnAAEffectProperty);
  RegisterComponentEditor(TCnAALabel, TCnAALabelEditor);
{$ENDIF}
end;

end.
