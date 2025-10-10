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

{*******************************************************************************
      �й�������ԭʼ��Ȩ����������Ԫ��������ũ�����ֲ���д���� Pascal ���룩
  _______________________________________________________________________

  �й������ࣨChinese Calendar Class (CCC)��
  �汾��v0.1��JavaScript�汾

  ��Ȩ���� (C) 2002-2003 neweroica (wy25@mail.bnu.edu.cn)

  ��ϵ��ʽ�� Email:  wy25@mail.bnu.edu.cn

             QQ: 32460746
  ________________________________________________________________________

*******************************************************************************}

{*******************************************************************************
  ����������ԭʼ��Ȩ����������Ԫ�������侫ȷ�������㲿�ֲ���д���� Pascal ���룩
  _______________________________________________________________________

  �������ǿ�Դ�ģ������ʹ�����е����ⲿ�ִ��룬�����������޸ġ������㷨(eph.js)��
  ����ũ���㷨(lunar.js)�й������ֵ����ݼ��㷨����һ���޸Ŀ���Ӱ����������׼ȷ�ԣ�
  ����������ѧ��̫�˽����ƾ�����������飬�벻Ҫ�Դ����κ��޸ģ�����Ū�ɳ�׾��

  ��������Լ������������ʹ���˱�����ĺ����㷨�����ݣ��������������������
  �����ݻ��㷨��Դ����������������Ҳ���Բ�������������������Ϊ��������Դ��
  �������壬�����ҹ���̽�֡�

  ���ߣ���ΰ�����������ʮ��ѧ��xunmeng04@163.com��13850262218
  ________________________________________________________________________

*******************************************************************************}

unit CnCalendar;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ��������㺯��
* ��Ԫ���ߣ�CnPack ������ (master@cnpack.org)
*           zjy (zjy@cnpack.org)
*           �޽���
* ��    ע�����ڡ�������ʱ��֧������Ф���������ڡ��������������С�ʮ�������񣩡�
*           ��Ԫ�����ˡ����ǡ���ʮ���ޡ����ס��žš�����������λ��ʵ�֣�
*           ������ũ������ת��Ҳ����ʵ�֡�
*
*           ע�⣬����Ԫ�еĹ�Ԫǰ�Ĺ�����ݳ�����˵������Ǿ���ֵ�为ֵ�����繫Ԫǰ 1 ��
*           ���� -1 �꣬û�й�Ԫ 0 �꣬�Ͳ��ֺ�������������ʹ�� 0 ��Ϊ��Ԫǰ 1 �겻ͬ��
*
*           �����ļ��㾫�ȼ���ʷ״�����ӵ�ԭ��ũ���ڹ�Ԫ 250 ��֮ǰ��׼ȷ���޷�
*           ȷ������ʷʵ�����һ�£�ʹ��ʱӦע�⡣
*
*           ��ũ��ת��Ŀǰ�����������Ԥ�����ݷ�ʽ����ֱ�������ڽ������㣬
*           ��������㷨�Ż�����ȷ�ȸ��ߵķ�ʽ�������������еľ�ȷ������㷨��
*           ��Ӱ��ũ����С���ж��������������˾����Ż��Ĺ�������Ҫ���º˶���ʷũ���ķ����Ƚڡ�
*
* ����ƽ̨��PWinXP SP2 + Delphi 2006
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2025.08.14 V2.9
*               �������ũ���¡��յ�ũ����ʮ�����㷨���ղ���������ţ
*           2025.05.27 V2.8
*               ���Ӳ��л�����ֲ�������������ľ�ȷ�����㷨������������֤ͨ��
*           2025.05.21 V2.7
*               ����������/Լ���������빫�������յĻ�ת�������ղ�������
*               ��������Ч��׼�����ڹ�Ԫǰ�����м���ƫ�������
*           2025.03.28 V2.6
*               ���Ӹ�֧���������ת�������������յļ��㣬������ʮ̫���ַ���
*           2025.02.20 V2.5
*               ������������ı��棬���� 2025 ��ũ�� 3 �µ�����ƫ������
*           2022.09.03 V2.4
*               �����޽��ʵı������֤�������¸�֧��С������ǰ��������������
*           2022.07.03 V2.3
*               �����޽��ʵı������֤������ 1582 �꼰֮ǰ������ʮ��ƫ�������
*           2022.01.29 V2.2
*               �����ո�����ÿ�ռ���λ�ļ��㣬��������ϲ�񡢸��񡢹���ȣ����й��������������Ĭ������
*           2018.08.22 V2.1
*               �޽��ʲ��� 2100 �굽 2800 ���ũ�����ݲ�Э�����������ռ����ƫ��
*           2018.07.18 V2.0
*               ����ͨ���㷨���¾��ǵļ��㣬���ӽ���������Ӽ������
*           2016.10.25 V1.9
*               ������ǵļ��㣬��������Ԫ������˾��ǡ�������ʱ����
*           2012.02.24 V1.8
*               ����һ��ȷ��Сʱ�����֧����ӿ�
*           2011.01.05 V1.7
*               �·ݵĽ����ֽ羫ȷ������
*           2011.01.05 V1.7
*               ����һ�·������ո�֧�������Сʱ������ʵ�� 23 ʱ���Ǵ��յĻ���
*           2010.04.12 V1.6
*               �����������г��ַ����ļ���
*           2009.07.27 V1.5
*               ����һ������ũ������ʱ����������ѭ��������
*           2009.07.16 V1.4
*               ����һ�����ռ��㲻��ȷ�����⣬���ӷ����ַ���
*           2008.04.16 V1.3
*               ���Ӹ�֧���������������С�ʮ�����ļ�����ũ�������ַ�����ת��
*           2007.11.15 V1.2
*               ���Ӷ�ʮ�����յļ���
*           2006.09.15 V1.1
*               ���ӹ�����ũ���Ĳ��ּ��㣬��ֲ���й�������
*           2005.12.18 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Math;

const
  SCnYinYangArray: array[0..1] of string =
    ('��', '��');
  {* �����ַ���}

  SCnTianGanArray: array[0..9] of string =
    ('��', '��', '��', '��', '��', '��', '��', '��', '��', '��');
  {* ����ַ�����Heavenly Stems}

  SCnDiZhiArray: array[0..11] of string =
    ('��', '��', '��', 'î', '��', '��', '��', 'δ', '��', '��', '��', '��');
  {* ��֧�ַ�����Earthly Branches}

  SCnShengXiaoArray: array[0..11] of string =
    ('��', 'ţ', '��', '��', '��', '��', '��', '��', '��', '��', '��', '��');
  {* ��Ф�ַ�����Zodiac Animals}

  SCnXingZuoArray: array[0..11] of string =
    ('����', '��ţ', '˫��', '��з', 'ʨ��', '��Ů',
     '���', '��Ы', '����', 'Ħ��', '��ƿ', '˫��');
  {* �����ַ�����Zodiac}

  SCn28XiuArray: array[0..27] of string =
    ('��', '��', 'ص', '��', '��', 'β', '��',  // ������������
     '��', 'ţ', 'Ů', '��', 'Σ', '��', '��',  // ������������
     '��', '¦', 'θ', '��', '��', '��', '��',  // �����׻�����
     '��', '��', '��', '��', '��', '��', '��'); // �Ϸ���ȸ����
  {* ��ʮ�����ַ���}

  SCn28XiuLongArray: array[0..27] of string =
    ('��ľ��', '������', 'ص����', '������', '���º�', 'β��', '��ˮ��',  // ������������
     '��ľ�', 'ţ��ţ', 'Ů����', '������', 'Σ����', '�һ���', '��ˮ��',  // ������������
     '��ľ��', '¦��', 'θ����', '���ռ�', '������', '�����', '��ˮԳ',  // �����׻�����
     '��ľ��', '�����', '�����', '������', '����¹', '�����', '��ˮ�'); // �Ϸ���ȸ����
  {* ��ʮ�������������ַ���}

  SCnLunarMonthLeapName: string = '��';
  SCnLunarMonthName: string = '��';
  SCnLunarMonthNameArray: array[0..11] of string =
    ('һ', '��', '��', '��', '��', '��', '��', '��', '��', 'ʮ', 'ʮһ', 'ʮ��');
  {* ũ���·��ַ���}

  SCnLunarNumber1Array: array[0..10] of string =
    ('һ', '��', '��', '��', '��', '��', '��', '��', '��', 'ʮ', '');
  {* ũ�����ڸ�λ�ַ���}

  SCnLunarNumber2Array: array[0..5] of string =
    ('��', 'ʮ', 'إ', 'ئ', '��', '��');
  {* ũ������ʮλ�ַ���}

  SCnWeekNumberArray: array[0..6] of string =
    ('��', 'һ', '��', '��', '��', '��', '��');
  {* �����ַ���}

  SCn5XingArray: array[0..4] of string =
    ('��', 'ľ', 'ˮ', '��', '��');
  {* �����ַ�������ͨ���Ľ�ľˮ����Ϊ˳��}

  SCn12JianArray: array[0..11] of string =
    ('��', '��', '��', 'ƽ', '��', 'ִ', '��', 'Σ', '��', '��', '��', '��');
  {* ʮ�����ַ���}

  SCn3FuArray: array[0..2] of string =
    ('����', '�з�', 'ĩ��');
  {* �����ַ���}

  SCnJieQiArray: array[0..23] of string = (
    '����', // ����  Beginning of Spring   3
    '��ˮ', // ����  Rain Water            4
    '����', // ����  Waking of Insects     5
    '����', // ����  March Equinox         6
    '����', // ����  Pure Brightness       7
    '����', // ����  Grain Rain            8
    '����', // ����  Beginning of Summer   9
    'С��', // ����  Grain Full            10
    'â��', // ����  Grain in Ear          11
    '����', // ����  Summer Solstice       12
    'С��', // ����  Slight Heat           13
    '����', // ����  Great Heat            14
    '����', // ����  Beginning of Autumn   15
    '����', // ����  Limit of Heat         16
    '��¶', // ����  White Dew             17
    '���', // ����  September Equinox     18
    '��¶', // ����  Cold Dew              19
    '˪��', // ����  Descent of Frost      20
    '����', // ����  Beginning of Winter   21
    'Сѩ', // ����  Slight Snow           22
    '��ѩ', // ����  Great Snow            23
    '����', // ����  Winter Solstice       24
    'С��', // ����  Slight Cold           1������һ�������еĵ�һ������
    '��'  // ����  Great Cold            2
  );
  {* �����ַ�����Solar Terms}

  SCn3YuanArray: array[0..2] of string =
    ( '��Ԫ', '��Ԫ', '��Ԫ' );
  {* ��Ԫ�����ַ���}

  SCn9XingArray: array[0..8] of string =
    ( 'һ��', '����', '����', '����', '���', '����', '�߳�', '�˰�', '����');
  {* ���������ַ���}

  SCn9Xing5XingArray: array[0..8] of string =
    ( 'ˮ', '��', 'ľ', 'ľ', '��', '��', '��', '��', '��');
  {* �����������������ַ���}

  SCn9XingStarArray: array[0..8] of string =
    ( '̰��', '����', '»��', '����', '����', '����', '�ƾ�', '��', '����');
  {* ���ǵ����������ַ���}

  SCn6YaoArray: array[0..5] of string =
    ('��ʤ', '����', '�ȸ�', '����', '��', '���');
  {* �����յ������ַ���}

  SCnTaiShen1Array: array[0..59] of string =
    ( 'ռ����', '��ĥ��', '����¯', '�ֿ���', '������',
      'ռ�Ŵ�', 'ռ��ĥ', '�����', '�ֿ�¯', '������',

      '�ż���', '��ĥ��', '������', '�ֿ��', '����¯',
      'ռ����', '��ĥ��', '���', '�ֿ���', '������',

      'ռ��¯', '��ĥ��', '������', '�ֿ���', '������',
      'ռ�Ų�', '��ĥ¯', '����¯', '�ֿ���', 'ռ����',

      'ռ����', '��ĥ��', '����¯', '�ֿ���', '������',
      'ռ�Ŵ�', 'ռ��ĥ', '�����', '�ֿ�¬', '������',

      '�ż���', '��ĥ��', '������', '�ֿ��', '�ֿ��',
      'ռ����', '��ĥ��', '���', '�ֿ���', '������',

      'ռ��¯', '��ĥ��', '������', '�ֿⴲ', '������',
      'ռ�Ų�', '��ĥ¯', '������', '�ֿ���', 'ռ�Ŵ�' );
  {* ÿ��̥��λ���ַ���������ʮ��֧���Ŷ�Ӧ}

  SCnTaiShen2Array: array[0..59] of string =
    ( '�ⶫ��', '�ⶫ��', '������', '������', '������',
      '������', '������', '������', '������', '������',

      '������', '������', '������', '������', '������',
      '������', '������', '������', '������', '������',

      '������', '������', '������', '������', '������',
      '������', '������', '������', '������', '���ڱ�',

      '���ڱ�', '���ڱ�', '���ڱ�', '���ڱ�', '������',
      '������', '������', '������', '������', '������',

      '���ڶ�', '���ڶ�', '���ڶ�', '���ڶ�', '���ڶ�',
      '�ⶫ��', '�ⶫ��', '�ⶫ��', '�ⶫ��', '�ⶫ��',

      '�ⶫ��', '������', '������', '������', '������',
      '������', '�ⶫ��', '�ⶫ��', '�ⶫ��', '�ⶫ��' );
  {* ÿ��̥��λ�ַ���������ʮ��֧���Ŷ�Ӧ}

  SCnNaYinWuXingArray: array[0..29] of string =
    ( '���н�', '¯�л�', '����ľ',
      '·����', '�����', 'ɽͷ��',

      '����ˮ', '��ǽ��', '������',
      '����ľ', 'Ȫ��ˮ', '������',

      '���׻�', '�ɰ�ľ', '����ˮ',
      'ɳ�н�', 'ɽ�»�', 'ƽ��ľ',

      '������', '�𲭽�', '��ƻ�',
      '���ˮ', '������', '���˽�',

      'ɣ��ľ', '��Ϫˮ', 'ɳ����',
      '���ϻ�', 'ʯ��ľ', '��ˮ' );
  {* ���������ַ�����������һ����ʮ��֧��Ӧ}

  SCnJiShenFangWeiArray: array[0..7] of string =
    ( '����', '����', '����', '����',
      '����', '����', '����', '����');
  {* ����λ�ַ�������Ӧ���Եİ˸�����
     �������ϲ�񡢲��񡢹��񣬹��񻹰�����������Ĭ��ָ����}

  SCnGanZhiArray: array[0..59] of string =
    ( '����', '�ҳ�', '����', '��î', '�쳽', '����', '����', '��δ', '����', '����',
      '����', '�Һ�', '����', '����', '����', '��î', '����', '����', '����', '��δ',
      '����', '����', '����', '����', '����', '����', '����', '��î', '�ɳ�', '����',
      '����', '��δ', '����', '����', '����', '����', '����', '����', '����', '��î',
      '�׳�', '����', '����', '��δ', '����', '����', '����', '����', '����', '���',
      '����', '��î', '����', '����', '����', '��δ', '����', '����', '����', '�ﺥ' );
  {* ��ʮ��֧�ַ�����Sexagenary Cycle}

  SCn12TaiSuiArray: array[0..11] of string =
    ( '̫��', '̫��', 'ɥ��', '̫��', '���', '����', '����', '����', '�׻�', '����', '�칷', '����');
  {* ʮ��̫�������ַ�������ʮ����֧��Ӧ}

  SCn60TaiSuiArray: array[0..59] of string =
    ( '���', '�²�', '����', '����', '�Դ�', '����', '����', '����', '����', '��־',
      'ʩ��', '�α�', '����', '����', '³��', '����', '����', '֣��', '½��', 'κ��',
      '����', '����', '����', '���', '����', '����', '����', '����', '��̩', '�쵥',
      '�´�', '����', '����', '�ƽ�', '����', 'л̫', '¬��', '����', '����', 'Ƥʱ',
      '���', '����', '����', '�ѱ�', '���', '�̱�', '����', 'Ҷ��', '���', '���',
      '�ų�', '����', '����', '����', '����', '����', 'ë��', 'ʯ��', '���', '�ݳ�' );
  {* ��ʮ̫�������ַ���������ʮ��֧��Ӧ}

type
  ECnDateTimeException = class(Exception);
  {* ��������쳣}

  TCnCalendarType = (ctinvalid, ctJulian, ctGregorian);
  {* �������ͣ�      �Ƿ���     ���ԣ�    �������}

  TCnLunarMonthType = (lmtSmall, lmtBig);
  {* ũ�������ͣ�      С�£�    ����}

  TCnEclipseType = (etNone, etSolar, etMoonFull, etMoonHalf);
  {* ����ʳ���ͣ�   �ޣ�    ��ʳ��   ��ȫʳ��    ��ƫʳ }

  TCnMoonPhase = (mpNone, mpShuo, mpWang);
  {* ���ࣺ       �ޣ�    ˷��    ��}

  TCnSunRiseSetType = (stNormal, stAllwaysUp, stAllwaysDown, stError);
  {* �ճ��������ͣ�    ��ͨ��    ���磬       ��ҹ��        ���ݴ��� }

function GetSunRiseSetTime(ADate: TDateTime; Longitude, Latitude: Extended;
  ZoneTime: Integer; out RiseTime, TransitTime, SetTime: TDateTime):
  TCnSunRiseSetType;
{* ����ĳ��γ�ȵص���ĳ�������ڵ��ճ�����ʱ�̡�

   ������
     ADate: TDateTime                     - ����
     Longitude: Extended                  - ����
     Latitude: Extended                   - γ��
     ZoneTime: Integer                    - �þ������ڵ�ʱ����������ڵľ�γ��Ӧ�� 8
     out RiseTime: TDateTime              - �����ճ�ʱ�䣬������ճ����� -1
     out TransitTime: TDateTime           - ��������ʱ�䣬��������з��� -1
     out SetTime: TDateTime               - ��������ʱ�䣬��������䷵�� -1

   ����ֵ��Boolean                        - �����ճ���������
}

function GetDateIsValid(AYear, AMonth, ADay: Integer): Boolean;
{* ���ع��������Ƿ�Ϸ���

   ������
     AYear, AMonth, ADay: Integer         - ���жϵĹ����ꡢ�¡���

   ����ֵ��Boolean                        - �����Ƿ�Ϸ�
}

procedure ValidDate(AYear, AMonth, ADay: Integer);
{* �жϹ��������Ƿ�Ϸ������Ϸ����׳��쳣��

   ������
     AYear, AMonth, ADay: Integer         - ���жϵĹ����ꡢ�¡���

   ����ֵ�����ޣ�
}

function GetLunarDateIsValid(ALunarYear, ALunarMonth, ALunarDay: Integer;
  IsLeapMonth: Boolean = False): Boolean;
{* ����ũ�������Ƿ�Ϸ���

   ������
     ALunarYear, ALunarMonth, ALunarDay: Integer          - ���жϵ�ũ���ꡢ�¡���
     IsLeapMonth: Boolean                                 - ��ũ�������Ƿ�����

   ����ֵ��Boolean                                        - �����Ƿ�Ϸ�
}

procedure ValidLunarDate(ALunarYear, ALunarMonth, ALunarDay: Integer;
  IsLeapMonth: Boolean = False);
{* �ж�ũ�������Ƿ�Ϸ������Ϸ����׳��쳣��

   ������
     ALunarYear, ALunarMonth, ALunarDay: Integer          - ���жϵ�ũ���ꡢ�¡���
     IsLeapMonth: Boolean                                 - ��ũ�������Ƿ�����

   ����ֵ�����ޣ�
}

function GetTimeIsValid(AHour, AMinitue, ASecond: Integer): Boolean;
{* ����ʱ���Ƿ�Ϸ���

   ������
     AHour, AMinitue, ASecond: Integer    - ���жϵ�ʱ���֡���

   ����ֵ��Boolean                        - �����Ƿ�Ϸ�
}

procedure ValidTime(AHour, AMinitue, ASecond: Integer);
{* �ж�ʱ���Ƿ�Ϸ������Ϸ����׳��쳣��

   ������
     AHour, AMinitue, ASecond: Integer    - ���жϵ�ʱ���֡���

   ����ֵ�����ޣ�
}

procedure StepToNextDay(var AYear, AMonth, ADay: Integer;
  ZeroYear: Boolean = False);
{* �������������󲽽�һ�죬���Ǹ������ꡢ���������ɾ 10 ������ء�֧�ֹ����� 0 ��
   ��-1 �����꣩�������� 0 �꣨0 �����꣩����ģʽ��Ĭ�ϲ�������ֹ��� 0 �ꡣ

   ������
     var AYear, AMonth, ADay: Integer     - �������Ĺ����ꡢ�¡���
     ZeroYear: Boolean                    - �Ƿ�������ֹ�Ԫ 0 ���Ա������ⳡ�ϼ���
}

function GetMonthDays(AYear, AMonth: Integer): Integer;
{* ȡ�������ĳ�������������� 1582 �� 10 �µ����������

   ������
     AYear, AMonth: Integer               - ĳ�����꼰ĳ���£������겻��Ϊ 0���紫 0 ����µ���ƽ��

   ����ֵ��Integer                        - ���ظ�������
}

function GetLunarMonthDays(ALunarYear, ALunarMonth: Integer;
  IsLeapMonth: Boolean = False): Integer;
{* ȡũ�����ĳ��������

   ������
     ALunarYear, ALunarMonth: Integer     - ĳũ���꼰ĳ���£�ũ���겻��Ϊ 0
     IsLeapMonth: Boolean                 - ��ũ�����Ƿ�����

   ����ֵ��Integer                        - ���ظ�������
}

function GetIsLeapYear(AYear: Integer): Boolean;
{* ����ĳ�����Ƿ����꣬�Զ��ж����������Ǹ������������֧�ֹ�Ԫǰ��
   ��Ԫǰ���������Ĺ�Ԫ�����ı���������ǰ�����ƣ�Ʃ�繫Ԫǰ 1 �ꡢǰ 5 �ꡭ��Ϊ���ꡣ

   ������
     AYear: Integer                       - ������Ĺ�����ݣ�����Ϊ 0

   ����ֵ��Boolean                        - �����Ƿ�����
}

function GetDayFromYearBegin(AYear, AMonth, ADay: Integer): Integer; overload;
{* ȡĳ�������ڵ������������������ 1582 �� 10 �µ�������������ֵ����Ϊ 0��

   ������
     AYear, AMonth, ADay: Integer         - ������Ĺ����ꡢ�¡���

   ����ֵ��Integer                        - ���ظ��յ����������
}

function GetDayFromYearBegin(AYear, AMonth, ADay, AHour: Integer;
  AMinute: Integer = 0; ASecond: Integer = 0): Extended; overload;
{* ȡĳ���ڵ������������Сʱ���֡�����������С���������� 1582 �� 10 �µ�������������ֵ����Ϊ 0��

   ������
     AYear, AMonth, ADay: Integer         - ������Ĺ����ꡢ�¡���
     AHour, AMinute, ASecond: Integer     - �������ʱ���֡���

   ����ֵ��Extended                       - ���ظ�ʱ�̵������������ʱ�������С��
}

function ExtractMonthDay(Days: Integer; AYear: Integer; out AMonth: Integer;
  out ADay: Integer): Boolean;
{* �Ӿ�������������º���������������ж��Ƿ������꣬���� False ��ʾ���Ϸ����ڡ�

   ������
     Days: Integer                        - ������ľ��������
     AYear: Integer                       - �����꣬�����ж��Ƿ�����
     out AMonth: Integer                  - ������
     out ADay: Integer                    - ������

   ����ֵ��Boolean                        - ���������Ƿ�Ϸ�
}

function GetWeek(const AValue: TDateTime): Integer; overload;
{* ���ĳ�������������ڼ���0-6 ��Ӧ�յ�����

   ������
     const AValue: TDateTime              - �����������

   ����ֵ��Integer                        - �������ڼ�
}

function GetWeek(AYear, AMonth, ADay: Integer): Integer; overload;
{* ���ĳ�������������ڼ���0-6 ��Ӧ�յ�����

   ������
     AYear, AMonth, ADay: Integer         - ������Ĺ����ꡢ�¡���

   ����ֵ��Integer                        - �������ڼ�
}

function GetWeekFromNumber(const AValue: Integer): string;
{* �����ֻ���������������������ڡ�����, 0-6 ��Ӧ�յ�����

   ������
     const AValue: Integer                - ���������������

   ����ֵ��string                         - ���������ַ���
}

function GetYinYangFromNumber(const AValue: Integer): string;
{* �����ֻ��������, 0-1 ��Ӧ������

   ������
     const AValue: Integer                - ���������������

   ����ֵ��string                         - ���������ַ���
}

function Get5XingFromNumber(const AValue: Integer): string;
{* �����ֻ��������, 0-4 ��Ӧ��ľˮ������

   ������
     const AValue: Integer                - ���������������

   ����ֵ��string                         - ���������ַ���
}

function Get12JianFromNumber(const AValue: Integer): string;
{* �����ֻ��ʮ������, 0-11 ��Ӧ������ƽ��ִ��Σ���տ��ա�

   ������
     const AValue: Integer                - �������ʮ��������

   ����ֵ��string                         - ����ʮ�����ַ���
}

function Get3FuFromNumber(const AValue: Integer): string;
{* �����ֻ��������, 0-2 ��Ӧ�����з�ĩ����

   ������
     const AValue: Integer                - ���������������

   ����ֵ��string                         - ���������ַ���
}

function GetTianGanFromNumber(const AValue: Integer): string;
{* �����ֻ�������, 0-9 ��Ӧ���ұ����켺�����ɹ

   ������
     const AValue: Integer                - ��������������

   ����ֵ��string                         - ��������ַ���
}

function GetDiZhiFromNumber(const AValue: Integer): string;
{* �����ֻ�õ�֧��, 0-11 ��Ӧ�ӳ���î������δ�����纥��

   ������
     const AValue: Integer                - ������ĵ�֧����

   ����ֵ��string                         - ���ص�֧�ַ���
}

function GetGanZhiFromNumber(const AValue: Integer): string;
{* �����ֻ����ɵ�֧��, 0-59 ��Ӧ�Ĳ�һһ�г��ˡ�

   ������
     const AValue: Integer                - ������ĸ�֧����

   ����ֵ��string                         - ���ظ�֧�ַ���
}

function Get12TaiSuiFromNumber(const AValue: Integer): string;
{* �ӵ�֧���ֻ��ʮ��̫����, 0-11 ��Ӧ̫�굽������

   ������
     const AValue: Integer                - ������ĵ�֧����

   ����ֵ��string                         - ����ʮ��̫���ַ���
}

function Get60TaiSuiFromNumber(const AValue: Integer): string;
{* �Ӹ�֧���ֻ����ʮ̫����, 0-59 ��Ӧ�Ĳ�һһ�г��ˡ�

   ������
     const AValue: Integer                - ������ĸ�֧����

   ����ֵ��string                         - ������ʮ̫���ַ���
}

function GetShengXiaoFromNumber(const AValue: Integer): string;
{* �����ֻ����Ф��, 0-11 ��Ӧ��ţ������������Ｆ����

   ������
     const AValue: Integer                - ���������Ф����

   ����ֵ��string                         - ������Ф�ַ���
}

function GetJieQiFromNumber(const AValue: Integer): string;
{* �����ֻ�ý�����, 0-23 ��Ӧ�Ĳ�һһ�г��ˡ�

   ������
     const AValue: Integer                - ������Ľ�������

   ����ֵ��string                         - ���ؽ����ַ���
}

function GetXingZuoFromNumber(const AValue: Integer): string;
{* �����ֻ��������, 0-11 ��Ӧ�Ĳ�һһ�г��ˡ�

   ������
     const AValue: Integer                - ���������������

   ����ֵ��string                         - ���������ַ���
}

function Get28XiuFromNumber(const AValue: Integer): string;
{* �����ֻ�ö�ʮ������, 0-27 ��Ӧ�Ĳ�һһ�г��ˡ�

   ������
     const AValue: Integer                - ������Ķ�ʮ��������

   ����ֵ��string                         - ���ض�ʮ�������ַ���
}

function Get28XiuLongFromNumber(const AValue: Integer): string;
{* �����ֻ�ö�ʮ����������, 0-27 ��Ӧ�Ĳ�һһ�г��ˡ�

   ������
     const AValue: Integer                - ������Ķ�ʮ��������

   ����ֵ��string                         - ���ض�ʮ�����������ַ���
}

function GetLunarMonthFromNumber(const AMonth: Integer; IsLeap: Boolean): string;
{* �����ֻ��ũ��������, 1-12��

   ������
     const AMonth: Integer                - �������ũ���·���
     IsLeap: Boolean                      - �Ƿ�����

   ����ֵ��string                         - ����ũ�����ַ���
}

function GetLunarDayFromNumber(const ADay: Integer): string;
{* �����ֻ��ũ��������, 1-30��

   ������
     const ADay: Integer                  - �������ũ������

   ����ֵ��string                         - ����ũ�����ַ���
}

function GetYinYangFromGan(const Gan: Integer): Integer;
{* ����ɻ��������, 0-9 ת���� 0-1��

   ������
     const Gan: Integer                   - ������������

   ����ֵ��Integer                        - ��������
}

function GetYinYangFromZhi(const Zhi: Integer): Integer;
{* �ӵ�֧���������, 0-11 ת���� 0-1��

   ������
     const Zhi: Integer                   - ������ĵ�֧��

   ����ֵ��Integer                        - ��������
}

function CombineGanZhi(Gan, Zhi: Integer): Integer;
{* ����ɵ�֧��ϳɸ�֧��0-9 0-11 ת���� 0-59��ע������ʮ���ţ���������������֧������ϡ�

   ������
     Gan, Zhi: Integer                    - ����ϵĸ�����֧��

   ����ֵ��Integer                        - ���ظ�֧����������ʧ���򷵻� -1
}

function ExtractGanZhi(GanZhi: Integer; out Gan: Integer; out Zhi: Integer): Boolean;
{* ����֧��ֳ���ɵ�֧��0-59 ת���� 0-9 �� 0-11��

   ������
     GanZhi: Integer                      - ����ֵĸ�֧��
     out Gan: Integer                     - ���ظ���
     out Zhi: Integer                     - ����֧��

   ����ֵ��Boolean                        - �����Ƿ��ֳɹ�
}

function Get5XingFromGan(const Gan: Integer): Integer;
{* ���ĳ�ɵ����У�0-4 ��Ӧ��ľˮ������

   ������
     const Gan: Integer                   - ������ĸ���

   ����ֵ��Integer                        - ��������
}

function Get5XingFromZhi(const Zhi: Integer): Integer;
{* ���ĳ֧�����У�0-4 ��Ӧ��ľˮ������

   ������
     const Zhi: Integer                   - �������֧��

   ����ֵ��Integer                        - ��������
}

function Get5XingFromGanZhi(const GanZhi: Integer): Integer; overload;
{* ���ĳ��֧���������У��̣���0-4 ��Ӧ��ľˮ������

   ������
     const GanZhi: Integer                - ������ĸ�֧��

   ����ֵ��Integer                        - ������������
}

function Get5XingFromGanZhi(Gan, Zhi: Integer): Integer; overload;
{* ���ĳ��֧���������У��̣���0-4 ��Ӧ��ľˮ����

   ������
     Gan, Zhi: Integer                    - ������ĸ�����֧��

   ����ֵ��Integer                        - ������������
}

function Get5XingFromDay(AYear, AMonth, ADay: Integer): Integer;
{* ���ĳ�����յ��������У��̣���0-4 ��Ӧ��ľˮ������

   ������
     AYear, AMonth, ADay: Integer         - ������Ĺ����ꡢ�¡���

   ����ֵ��Integer                        - ������������
}

function Get5XingLongFromGanZhi(const GanZhi: Integer): string; overload;
{* ���ĳ��֧���������У������������ַ�����

   ������
     const GanZhi: Integer                - ������ĸ�֧��

   ����ֵ��string                         - �������������ַ���
}

function Get5XingLongFromGanZhi(Gan, Zhi: Integer): string; overload;
{* ���ĳ��֧���������У������������ַ�����

   ������
     Gan, Zhi: Integer                    - ������ĸ�֧��

   ����ֵ��string                         - �����������г��ַ���
}

function Get5XingLongFromDay(AYear, AMonth, ADay: Integer): string;
{* ���ĳ�����յ��������У������������ַ�����

   ������
     AYear, AMonth, ADay: Integer         - ������Ĺ����ꡢ�¡���

   ����ֵ��string                         - �����������г��ַ���
}

function Get3HeFromZhi(const Zhi: Integer; out He1: Integer;
  out He2: Integer): Boolean;
{* ���ĳ��֧�������������ϡ�

   ������
     const Zhi: Integer                   - ������ĵ�֧��
     out He1: Integer                     - �������ϵ�֧֮һ
     out He2: Integer                     - �������ϵ�֧֮��

   ����ֵ��Boolean                        - �����Ƿ����ɹ�
}

function GetGanZhiFromHour(AYear, AMonth, ADay, AHour: Integer): Integer;
{* ���ĳ����ʱ����ɵ�֧��0-59 ��Ӧ ���ӵ��ﺥ��

   ������
     AYear, AMonth, ADay, AHour: Integer  - ������Ĺ����ꡢ�¡��ա�ʱ

   ����ֵ��Integer                        - ����ʱ��֧
}

function GetGanZhiFromDay(AYear, AMonth, ADay: Integer): Integer; overload;
{* ���ĳ�����յ���ɵ�֧��0-59 ��Ӧ���ӵ��ﺥ��

   ������
     AYear, AMonth, ADay: Integer         - ������Ĺ����ꡢ�¡���

   ����ֵ��Integer                        - �����ո�֧
}

function GetGanZhiFromDay(AYear, AMonth, ADay, AHour: Integer): Integer; overload;
{* ���ĳ�����յ���ɵ�֧��0-59 ��Ӧ ���ӵ��ﺥ��Сʱ���������ж� 23 Сʱ���Ǵ��ա�

   ������
     AYear, AMonth, ADay, AHour: Integer  - ������Ĺ����ꡢ�¡��ա�ʱ

   ����ֵ��Integer                        - �����ո�֧
}

function GetGanZhiFromDay(AllDays: Integer): Integer; overload;
{* �����ʽΪ����������ĳ�����յ���ɵ�֧��0-59 ��Ӧ���ӵ��ﺥ������Ϊ���빫ԪԪ�� 1 �� 0 �յľ���������

   ������
     AllDays: Integer                     - ������Ĺ�����������

   ����ֵ��Integer                        - �����ո�֧
}

function GetGanZhiFromMonth(AYear, AMonth, ADay: Integer): Integer; overload;
{* ���ĳ�����µ���ɵ�֧����Ҫ������Ϊ���Խ����ֽ磬������ʱ��0-59 ��Ӧ���ӵ��ﺥ��

   ������
     AYear, AMonth, ADay: Integer         - ������Ĺ����ꡢ�¡���

   ����ֵ��Integer                        - �����¸�֧
}

function GetGanZhiFromMonth(AYear, AMonth, ADay, AHour: Integer): Integer; overload;
{* ���ĳ�����µ���ɵ�֧����Ҫ����ʱ����Ϊ���Խ����ֽ硣0-59 ��Ӧ���ӵ��ﺥ��

   ������
     AYear, AMonth, ADay, AHour: Integer  - ������Ĺ����ꡢ�¡��ա�ʱ

   ����ֵ��Integer                        - �����¸�֧
}

function GetGanZhiFromYear(AYear: Integer): Integer; overload;
{* ���ĳ��/ũ�������ɵ�֧��0-59 ��Ӧ���ӵ��ﺥ��

   ������
     AYear: Integer                       - ������Ĺ������ũ����

   ����ֵ��Integer                        - �������֧
}

function GetGanZhiFromYear(AYear, AMonth, ADay: Integer): Integer; overload;
{* ���ݹ��������ջ��ĳ���������ɵ�֧��������Ϊ��ֽ磬0-59 ��Ӧ���ӵ��ﺥ��

   ������
     AYear, AMonth, ADay: Integer         - ������Ĺ����ꡢ�¡���

   ����ֵ��Integer                        - �������֧
}

function GetGanZhiFromYear(AYear, AMonth, ADay, AHour: Integer): Integer; overload;
{* ���ݹ���������ʱ���ĳ���������ɵ�֧��������Ϊ��ֽ磬��ȷ��Сʱ��0-59 ��Ӧ���ӵ��ﺥ��

   ������
     AYear, AMonth, ADay, AHour: Integer  - ������Ĺ����ꡢ�¡��ա�ʱ

   ����ֵ��Integer                        - �������֧
}

function GetGanFromYear(AYear: Integer): Integer;
{* ���ĳ��/ũ�������ɣ�0-9 ��Ӧ�׵��

   ������
     AYear: Integer                       - ������Ĺ������ũ����

   ����ֵ��Integer                        - �������
}

function GetZhiFromYear(AYear: Integer): Integer;
{* ���ĳ��/ũ����ĵ�֧��0-11 ��Ӧ�ӵ���

   ������
     AYear: Integer                       - ������Ĺ������ũ����

   ����ֵ��Integer                        - ���ص�֧
}

function GetShengXiaoFromYear(AYear: Integer): Integer;
{* ���ĳ��/ũ�������ФҲ���ǵ�֧��0-11 ��Ӧ����

   ������
     AYear: Integer                       - ������Ĺ�����

   ����ֵ��Integer                        - ������Ф
}

function GetXingZuoFromMonthDay(AMonth, ADay: Integer): Integer;
{* ���ĳ�������յ�������0-11 ��Ӧ����˫�㡣

   ������
     AMonth, ADay: Integer                - ������Ĺ����¡���

   ����ֵ��Integer                        - ��������
}

function Get12JianFromDay(AYear, AMonth, ADay: Integer): Integer;
{* ���ĳ�������յ�ʮ������0-11 ��Ӧ�����ա�

   ������
     AYear, AMonth, ADay: Integer         - ������Ĺ����ꡢ�¡���

   ����ֵ��Integer                        - ����ʮ����
}

function Get28XiuFromDay(AYear, AMonth, ADay: Integer): Integer;
{* ���ĳ�����յĶ�ʮ���ޣ�0-27 ��Ӧ�ǵ�����

   ������
     AYear, AMonth, ADay: Integer         - ������Ĺ����ꡢ�¡���

   ����ֵ��Integer                        - ���ض�ʮ����
}

function GetLunar28XiuFromDay(AYear, AMonth, ADay: Integer): Integer;
{* ���ĳ�����յ�ũ����ʮ���ޣ�0-27 ��Ӧ�ǵ�������ũ���ղ����ڻ򳬽磬���� -1
   ע���ũ����ʮ���޵Ľ���в����� 8 ţ��

   ������
     AYear, AMonth, ADay: Integer         - ������Ĺ����ꡢ�¡���

   ����ֵ��Integer                        - ����ũ����ʮ����
}

function GetTaiShenStringFromDay(AYear, AMonth, ADay: Integer): string; overload;
{* ���ĳ�����յ�̥��λ��0-59 ����̥��λ�ü�̥��λ���ַ�����

   ������
     AYear, AMonth, ADay: Integer         - ������Ĺ����ꡢ�¡���

   ����ֵ��string                         - ����̥��λ�ַ���
}

function GetTaiShenStringFromDay(AYear, AMonth, ADay: Integer;
  out TaiShen1: string; out TaiShen2: string): Boolean; overload;
{* ���ĳ�����յ�̥��λ��0-59 ����̥��λ����̥��λ�����ַ�����

   ������
     AYear, AMonth, ADay: Integer         - ������Ĺ����ꡢ�¡���
     out TaiShen1: string                 - ����̥��λ���ַ���
     out TaiShen2: string                 - ����̥��λ�ַ���

   ����ֵ��Boolean                        - �����Ƿ��ȡ�ɹ�
}

function GetShiChenFromHour(AHour: Integer): Integer;
{* ���Сʱʱ�̶�Ӧ��ʱ����0-11 ��Ӧ��������

   ������
     AHour: Integer                       - �������Сʱ��

   ����ֵ��Integer                        - ����ʱ��
}

function AdjustYearToGanZhi(var AYear: Integer; AMonth: Integer;
  ADay: Integer; AHour: Integer): Boolean;
{* ��������Ϊ�磬����������������յ����������׼��֧���꣬�������������ĸ�֧
   �ȸ���ļ��㡣ע�ⰴԼ���Ĺ����������� 0 ʱ����������꣬������������ʱ��
   �� 0 ʱ���ĳһʱ�̣��·ݷֽ�Ľ���Ҳ���ơ�

   ������
     var AYear: Integer                   - �������Ĺ����꣬������Ľ��Ҳ������
     AMonth: Integer                      - �ù������ڵ��·���
     ADay: Integer                        - �ù������ڵ�����
     AHour: Integer                       - �ù������ڵ�Сʱ��

   ����ֵ��Boolean                        - ���������Ƿ�Ϸ���ע�����Ƿ�����޹�
}

function AdjustYearMonthToGanZhi(var AYear: Integer; var AMonth: Integer;
  ADay: Integer; AHour: Integer): Boolean;
{* �������������Ϊ�磬����������������յ���������·�������׼��֧���꣬
   ������������µĸ�֧�ȸ���ļ��㡣ע�ⰴԼ���Ĺ����������� 0 ʱ����������꣬
   ������������ʱ���� 0 ʱ���ĳһʱ�̣��·ݷֽ�Ľ���Ҳ���ơ�

   ������
     var AYear: Integer                   - �������Ĺ����꣬������Ľ��Ҳ������
     var AMonth: Integer                  - �������Ĺ����£�������Ľ��Ҳ������
     ADay: Integer                        - �ù������ڵ�����
     AHour: Integer                       - �ù������ڵ�Сʱ��

   ����ֵ��Boolean                        - ���������Ƿ�Ϸ���ע�����Ƿ�����޹�
}

function Get3YuanFromNumber(A3Yuan: Integer): string;
{* �����ֻ����Ԫ���ƣ�0-2��

   ������
     A3Yuan: Integer                      - ����ȡ����Ԫ����

   ����ֵ��string                         - ������Ԫ����
}

function Get9XingFromNumber(A9Xing: Integer): string;
{* �����ֻ�þ������ƣ�0-8��

   ������
     A9Xing: Integer                      - ����ȡ�ľ�������

   ����ֵ��string                         - ���ؾ�������
}

function Get6YaoFromNumber(A6Yao: Integer): string;
{* �����ֻ���������ƣ�0-5��

   ������
     A6Yao: Integer                       - ����ȡ����������

   ����ֵ��string                         - ������������
}

function Get3YuanFromYear(AYear, AMonth, ADay: Integer): Integer;
{* ��ȡ��������������Ԫ��0-2��

   ������
     AYear, AMonth, ADay: Integer         - ������Ĺ����ꡢ�¡���

   ����ֵ��Integer                        - ������Ԫ
}

function GetYun9XingFromYear(AYear, AMonth, ADay: Integer): Integer;
{* ��ȡ��������˾��ǣ�0-8 ��Ӧһ�׵����ϡ�

   ������
     AYear, AMonth, ADay: Integer         - ������Ĺ����ꡢ�¡���

   ����ֵ��Integer                        - �����˾���
}

function Get9XingFromYear(AYear, AMonth, ADay: Integer): Integer;
{* ��ȡ�����������ǣ�0-8 ��Ӧһ�׵����ϡ�

   ������
     AYear, AMonth, ADay: Integer         - ������Ĺ����ꡢ�¡���

   ����ֵ��Integer                        - ���������
}

function Get9XingFromMonth(AYear, AMonth, ADay: Integer): Integer;
{* ��ȡ�����µ��¾��ǣ�0-8 ��Ӧһ�׵����ϡ�

   ������
     AYear, AMonth, ADay: Integer         - ������Ĺ����ꡢ�¡���

   ����ֵ��Integer                        - �����¾���
}

function Get9XingFromDay(AYear, AMonth, ADay: Integer): Integer;
{* ��ȡ�����յ��վ��ǣ�0-8 ��Ӧһ�׵����ϡ�

   ������
     AYear, AMonth, ADay: Integer         - ������Ĺ����ꡢ�¡���

   ����ֵ��Integer                        - �����վ���
}

function Get9XingFromHour(AYear, AMonth, ADay, AHour: Integer): Integer;
{* ��ȡ����ʱ��ʱ���ǣ�0-8 ��Ӧһ�׵����ϡ�

   ������
     AYear, AMonth, ADay, AHour: Integer  - ������Ĺ����ꡢ�¡��ա�ʱ

   ����ֵ��Integer                        - ����ʱ����
}

function Get6YaoFromDay(AYear, AMonth, ADay: Integer): Integer;
{* ��ȡ�����յ������ף�0-5 ��Ӧ��ʤ����ڡ�

   ������
     AYear, AMonth, ADay: Integer         - ������Ĺ����ꡢ�¡���

   ����ֵ��Integer                        - ����������
}

function GetJiShenFangWeiFromNumber(AFangWei: Integer): string;
{* ���ݼ��񣨰�������ϲ�񡢸��񡢹��񣩷�λ���ֻ�ü���λ���ơ�

   ������
     AFangWei: Integer                    - ����ȡ�ļ���λ��

   ����ֵ��string                         - ���ؼ���λ����
}

function GetCaiShenFangWeiFromDay(AYear, AMonth, ADay: Integer): Integer;
{* ��ù��������յĲ���λ��0-7��

   ������
     AYear, AMonth, ADay: Integer         - ������Ĺ����ꡢ�¡���

   ����ֵ��Integer                        - ���ز���λ
}

function GetXiShenFangWeiFromDay(AYear, AMonth, ADay: Integer): Integer;
{* ��ù��������յ�ϲ��λ��0-7��

   ������
     AYear, AMonth, ADay: Integer         - ������Ĺ����ꡢ�¡���

   ����ֵ��Integer                        - ����ϲ��λ
}

function GetFuShenFangWeiFromDay(AYear, AMonth, ADay: Integer): Integer;
{* ��ù��������յĸ���λ��0-7��

   ������
     AYear, AMonth, ADay: Integer         - ������Ĺ����ꡢ�¡���

   ����ֵ��Integer                        - ���ظ���λ
}

function GetGuiShenFangWeiFromDay(AYear, AMonth, ADay: Integer): Integer;
{* ��ù��������յĹ���λ��0-7��Ĭ��Ϊ����

   ������
     AYear, AMonth, ADay: Integer         - ������Ĺ����ꡢ�¡���

   ����ֵ��Integer                        - ���ع���λ��Ĭ��Ϊ����
}

function GetYangGuiShenFangWeiFromDay(AYear, AMonth, ADay: Integer): Integer;
{* ��ù��������յ�������λ��0-7��

   ������
     AYear, AMonth, ADay: Integer         - ������Ĺ����ꡢ�¡���

   ����ֵ��Integer                        - ����������λ
}

function GetYingShenFangWeiFromDay(AYear, AMonth, ADay: Integer): Integer;
{* ��ù��������յ�������λ��0-7��

   ������
     AYear, AMonth, ADay: Integer         - ������Ĺ����ꡢ�¡���

   ����ֵ��Integer                        - ����������λ
}

function GetAllDays(AYear, AMonth, ADay: Integer): Integer;
{* ��þ๫ԪԪ�� 1 �� 0 �յľ���������

   ������
     AYear, AMonth, ADay: Integer         - ������Ĺ����ꡢ�¡���

   ����ֵ��Integer                        - ���ؾ�������
}

function GetJieQiInAYear(AYear, N: Integer; out AMonth: Integer;
  out ADay: Integer; out AHour: Integer; out AMinitue: Integer; out ASecond: Integer): Boolean;
{* ���ĳ�������ڵĵ� N �������Ľ�������ʱ���룬0~23����Ӧ����˳���С���������������������󺮡�

   ������
     AYear: Integer                       - ������Ĺ�����
     N: Integer                           - ������Ľ������
     out AMonth: Integer                  - ���ؽ��������·�
     out ADay: Integer                    - ���ؽ���������
     out AHour: Integer                   - ���ؽ�������ʱ�̵�Сʱ��
     out AMinitue: Integer                - ���ؽ�������ʱ�̵ķ�����
     out ASecond: Integer                 - ���ؽ�������ʱ�̵�����

   ����ֵ��Boolean                        - ���ؼ����Ƿ�ɹ�
}

function GetJieQiFromDay(AYear, AMonth, ADay: Integer): Integer;
{* ��ù����������Ǳ����ʲô������0-23����Ӧ�������󺮣����򷵻� -1��

   ������
     AYear, AMonth, ADay: Integer         - ������Ĺ����ꡢ�¡���

   ����ֵ��Integer                        - ���ؽ�����ţ�-1 Ϊ���ǽ���
}

function GetJieQiTimeFromDay(AYear, AMonth, ADay: Integer;
  out AHour: Integer; out AMinitue: Integer; out ASecond: Integer): Integer;
{* ��ù����������Ǳ����ʲô�����Լ�����ʱ�̣�0-23����Ӧ�������󺮣����򷵻� -1��

   ������
     AYear, AMonth, ADay: Integer         - ������Ĺ����ꡢ�¡���
     out AHour: Integer                   - ���ؽ�������ʱ�̵�Сʱ��
     out AMinitue: Integer                - ���ؽ�������ʱ�̵ķ�����
     out ASecond: Integer                 - ���ؽ�������ʱ�̵�����

   ����ֵ��Integer                        - ���ؽ�����ţ�-1 Ϊ���ǽ���
}

function GetShu9Day(AYear, AMonth, ADay: Integer; out JiuSeq: Integer; out JiuDay: Integer): Boolean;
{* ��ù������������������еĵڼ��ŵĵڼ��գ�1~9,1~9 ��Ӧһ�ŵ��žţ����� False Ϊ�����������ڡ�

   ������
     AYear, AMonth, ADay: Integer         - ������Ĺ����ꡢ�¡���
     out JiuSeq: Integer                  - �������ŵĵڼ���
     out JiuDay: Integer                  - ���ر����ڵڼ���

   ����ֵ��Boolean                        - �����Ƿ�����������
}

function Get3FuDay(AYear, AMonth, ADay: Integer; out FuSeq: Integer; out FuDay: Integer): Boolean;
{* ��ù������������������еĵڼ����ĵڼ��գ�0~2,1~10���� 20����Ӧ������ĩ���ķ��գ����� False Ϊ�����������ڡ�

   ������
     AYear, AMonth, ADay: Integer         - ������Ĺ����ꡢ�¡���
     out FuSeq: Integer                   - ���������ĵڼ���
     out FuDay: Integer                   - ���ر����ڵڼ���

   ����ֵ��Boolean                        - �����Ƿ�����������
}

function GetRuMeiDay(AYear: Integer; out AMonth: Integer; out ADay: Integer): Boolean;
{* ���ĳ�������е���÷���ڣ�÷�꼾�ڵĿ�ʼ�գ�â�ֺ�ĵ�һ�����գ������Ƿ��ȡ�ɹ���

   ������
     AYear: Integer                       - ������Ĺ�����
     out AMonth: Integer                  - ���ص���÷���������·�
     out ADay: Integer                    - ���ص���÷��

   ����ֵ��Boolean                        - �����Ƿ��ȡ�ɹ�
}

function GetChuMeiDay(AYear: Integer; out AMonth: Integer; out ADay: Integer): Boolean;
{* ���ĳ�������еĳ�÷���ڣ�÷�꼾�ڵĽ����գ�С���ĵ�һ��δ�գ������Ƿ��ȡ�ɹ���

   ������
     AYear: Integer                       - ������Ĺ�����
     out AMonth: Integer                  - ���صĳ�÷���������·�
     out ADay: Integer                    - ���صĳ�÷��

   ����ֵ��Boolean                        - �����Ƿ��ȡ�ɹ�
}

function GetLunarFromDay(AYear, AMonth, ADay: Integer;
  out LunarYear, LunarMonth, LunarDay: Integer; out IsLeapMonth: Boolean): Boolean;
{* ���ĳ���������յ�ũ�������պ��Ƿ����£������겻��Ϊ 0�������Ƿ��ȡ�ɹ���
   ע������ʷ�ϵ�ȷ���ֹ����������·ݵģ����������á����¡������ڶ����¡�
   ���磺
   ��Ԫ 239 �� 12 �� 13 ��ʮ���´�󣬹�Ԫ 240 �� 1 �� 12 ������ʮ����С�������������£�
   ��Ϊ����ʾ���֣�240 �� 1 �� 12 �ռ��Ժ��ʮ���·��� IsLeapMonth Ϊ True��
   ��Ԫ 23 �� 12 �� 2 ��ʮ����С��12 �� 31 ������ʮ���´�Ҳ�������£�
   ͬ��Ϊ����ʾ���֣�12 �� 31 �ռ��Ժ��ʮ���·��� IsLeapMonth Ϊ True��

   ������
     AYear, AMonth, ADay: Integer                         - ������Ĺ����ꡢ�¡���
     out LunarYear, LunarMonth, LunarDay: Integer         - �������ɼ�������ũ���ꡢ�¡���
     out IsLeapMonth: Boolean                             - �����Ƿ�����

   ����ֵ��Boolean                                        - �����Ƿ����ɹ�
}

function GetLunarMonthDayFromDay(AYear, AMonth, ADay: Integer;
  out LunarMonth, LunarDay: Integer; out IsLeapMonth: Boolean): Boolean;
{* ���ĳ���������յ�ũ�����պ��Ƿ����µ���Ϣ����ݰ���ȴ���ʵ�ʿ��ܲ��ȡ������Ƿ��ȡ�ɹ���

   ������
     AYear, AMonth, ADay: Integer         - ������Ĺ����ꡢ�¡���
     out LunarMonth, LunarDay: Integer    - �������ɼ�������ũ���¡���
     out IsLeapMonth: Boolean             - �����Ƿ�����

   ����ֵ��Boolean                        - �����Ƿ����ɹ�
}

function GetLunarLeapMonth(AYear: Integer): Integer;
{* ���ĳũ����ĵڼ����������£����� 1~12 ��Ӧ��һ���µ���ʮ�����£�
   Ҳ����ȥ����ʮ���µ�������ʮһ�£����� 0 ��ʾ�����¡�

   ������
     AYear: Integer                       - �������ũ���꣬������ͬ�ڹ����꣬�޹�Ԫ 0 �꣬������� 0 �ⲻ���ڵ���Ҳ����������

   ����ֵ��Integer                        - �������£�0 ��ʾ������
}

function GetDayFromLunar(ALunarYear, ALunarMonth, ALunarDay: Integer; IsLeapMonth:
  Boolean; out AYear, AMonth, ADay: Integer): Boolean;
{* ���ĳũ�������գ����Ƿ����£��Ĺ��������գ�ũ���겻��Ϊ 0�������Ƿ��ȡ�ɹ���
   ע������ʷ�ϵ�ȷ���ֹ����������·ݵġ����磺
   ��Ԫ 239 �� 12 �� 13 ��ʮ���´�󣬹�Ԫ 240 �� 1 �� 12 ������ʮ����С���ֲ������£����ũ�� 239 ��ʮ�����ڵľͲ鲻������
   ��Ԫ 23 �� 12 �� 2 ��ʮ����С��12 �� 31 ������ʮ���´�Ҳ�������£����ũ�� 23 ��ʮ�����ڵ�Ҳ�鲻������

   ������
     ALunarYear, ALunarMonth, ALunarDay: Integer          - �������ũ���ꡢ�¡��գ�ũ���겻��Ϊ 0
     IsLeapMonth: Boolean                                 - �Ƿ�����
     out AYear, AMonth, ADay: Integer                     - �������ɼ��������ꡢ�¡���

   ����ֵ��Boolean                                        - �����Ƿ����ɹ�
}

function Compare2Day(Year1, Month1, Day1, Year2, Month2, Day2: Integer): Integer;
{* �Ƚ������������ڣ�ǰ�ߴ��ڡ����ڡ�С�ں���ʱ�ֱ𷵻� 1��0��-1������ָ������δ����

   ������
     Year1, Month1, Day1: Integer         - ���жϵĵ�һ�������ꡢ�¡���
     Year2, Month2, Day2: Integer         - ���жϵĵڶ��������ꡢ�¡���

   ����ֵ��Integer                        - ���رȽϽ��
}

function Compare2LunarDay(Year1, Month1, Day1: Integer; IsLeap1: Boolean;
  Year2, Month2, Day2: Integer; IsLeap2: Boolean): Integer;
{* �Ƚ�����ũ�����ڣ�����������Ϣ����ǰ�ߴ��ڡ����ڡ�С�ں���ʱ�ֱ𷵻� 1��0��-1������ָ������δ����

   ������
     Year1, Month1, Day1: Integer         - ���жϵĵ�һ��ũ���ꡢ�¡���
     IsLeap1: Boolean                     - ��һ��ũ���Ƿ�����
     Year2, Month2, Day2: Integer         - ���жϵĵڶ���ũ���ꡢ�¡���
     IsLeap2: Boolean                     - �ڶ���ũ���Ƿ�����

   ����ֵ��Integer                        - ���رȽϽ��
}

function GetYearSeperatedByLiChun(AYear, AMonth, ADay: Integer): Integer;
{* ���ݹ��������գ����ظ����������������ָ����ݣ�Ҳ����˵�����պ��ǽ��꣬����Ϊȥ�ꡣ

   ������
     AYear, AMonth, ADay: Integer         - ������Ĺ����ꡢ�¡���

   ����ֵ��Integer                        - ��������������
}

function GetEquStandardDays(AYear, AMonth, ADay: Integer): Integer;
{* ���ĳ�����յĵ�Ч��׼�������ƺ����������������ǰ�Ƶ���������ԪԪ��Ԫ�� 0 �յ�������
   ע�������ڲ��������ⳡ�ϵ�Ҫ�󣬸ú�������������������ҳ��� 0��Ҳ����Ԫǰ 1 ��Ϊ 0��

   ������
     AYear, AMonth, ADay: Integer         - ������Ĺ����ꡢ�¡���

   ����ֵ��Integer                        - ���ص�Ч��׼����
}

function GetDayFromEquStandardDays(EquDays: Integer;
  out AYear, AMonth, ADay: Integer): Boolean;
{* ��õ�Ч��׼������Ӧ��ĳ�����գ������Ƿ��ȡ�ɹ���
   Ŀǰ��֧�ֹ�ԪԪ��Ԫ�� 2 ��֮ǰ������Ҳ���ǲ�֧�ָ��ĵ�Ч��׼�ա�
   ע�������ڲ��������ⳡ�ϵ�Ҫ�󣬸ú������ص����������ҳ��� 0��Ҳ����Ԫǰ 1 ��Ϊ 0��

   ������
     EquDays: Integer                     - ������ĵ�Ч��׼����
     out AYear, AMonth, ADay: Integer     - �������ɼ��������ꡢ�¡���

   ����ֵ��Boolean                        - �����Ƿ����ɹ�
}

function GetJulianDate(AYear, AMonth, ADay: Integer): Extended; overload;
{* ���ĳ���������� 12 �������������Ҳ�����������Ĺ�Ԫǰ 4713 �� 1 �� 1 ��
   ���� 12 ��Ϊ�������������������������꣩��һ���Ǹ�������

   ������
     AYear, AMonth, ADay: Integer         - ������Ĺ����ꡢ�¡���

   ����ֵ��Extended                       - ������������
}

function GetJulianDate(AYear, AMonth, ADay: Integer;
  AHour, AMinute, ASecond: Integer): Extended; overload;
{* ���ĳ��������ʱ�̵�����������

   ������
     AYear, AMonth, ADay: Integer         - ������Ĺ����ꡢ�¡���
     AHour, AMinute, ASecond: Integer     - �������ʱ���֡���

   ����ֵ��Extended                       - ������������
}

function GetModifiedJulianDate(AYear, AMonth, ADay: Integer): Extended; overload;
{* ���ĳ���������� 12 ���Լ������������Ҳ���Ը���������Ĺ�Ԫ 1858 �� 11 �� 17 ��
   0 ��Ϊ����������С������һ���� 0.5��

   ������
     AYear, AMonth, ADay: Integer         - ������Ĺ����ꡢ�¡���

   ����ֵ��Extended                       - ����Լ����������
}

function GetModifiedJulianDate(AYear, AMonth, ADay: Integer;
  AHour, AMinute, ASecond: Integer): Extended; overload;
{* ���ĳ��������ʱ�̵�Լ������������

   ������
     AYear, AMonth, ADay: Integer         - ������Ĺ����ꡢ�¡���
     AHour, AMinute, ASecond: Integer     - �������ʱ���֡���

   ����ֵ��Extended                       - ����Լ����������
}

function GetDayFromJulianDate(JD: Extended; out AYear, AMonth, ADay: Integer): Boolean;
{* ���ĳ����������Ӧ��һ��Ĺ��������գ�������Ӧ�������硣

   ������
     JD: Extended                         - �����������������Ӧ������������ֵ
     out AYear, AMonth, ADay: Integer     - ���صĹ����ꡢ�¡���

   ����ֵ��Boolean                        - �����Ƿ����ɹ�
}

function GetDayFromModifiedJulianDate(MJD: Extended; out AYear, AMonth, ADay: Integer): Boolean;
{* ���ĳԼ������������Ӧ�Ĺ��������գ�С������Ӧ���� 0.5����Ӧ�������硣

   ������
     MJD: Extended                        - �������Լ������������С������Ӧ���� 0.5
     out AYear, AMonth, ADay: Integer     - ���صĹ����ꡢ�¡���

   ����ֵ��Boolean                        - �����Ƿ����ɹ�
}

implementation

resourcestring
  SCnErrorDateIsInvalid = 'Date is Invalid: %d-%d-%d.';
  SCnErrorLunarDateIsInvalid = 'Lunar Date is Invalid: %d-%d-%d, MonthLeap %d.';
  SCnErrorConvertLunarDate = 'Date is Invalid for Lunar Conversion: %d-%d-%d.';
  SCnErrorTimeIsInvalid = 'Time is Invalid: %d:%d:%d.';
  SCnErrorYearIsInvalid = 'Year is Invalid: 0';

const
  RADS = 0.0174532925;

  SCnTaiXuanPeiShuArray: array[0..5] of Integer =
    (9, 8, 7, 6, 5, 4);
  {* ��֧��̫���������飬���ڲ������֧��̫�������Ӷ���������������}

  SCnLunar28XiuNumber: array[1..12] of array[1..30] of Byte = (
  {* ũ���¡��ն�Ӧ�Ķ�ʮ�������飬������á�ע����ͷû�� 8 ţ}
    (12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 0, 1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14),
    (14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 0, 1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15, 16),
    (16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 0, 1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18),
    (18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 0, 1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20),
    (20, 21, 22, 23, 24, 25, 26, 27, 0, 1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22),
    (22, 23, 24, 25, 26, 27, 0, 1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24),
    (25, 26, 27, 0, 1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27),
    (0, 1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 0, 1, 2),
    (2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 0, 1, 2, 3, 4),
    (4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 0, 1, 2, 3, 4, 5, 6),
    (7, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 0, 1, 2, 3, 4, 5, 6, 7, 9, 10),
    (10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 0, 1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12)
  );

  SCnLeapNumber: array[0..3648] of Integer = (
    0, 1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 5, 5, 5, 6, 6, 6, 7, 7, 8, 8, 8,
    9, 9, 9, 10, 10, 10, 11, 11, 12, 12, 12, 13, 13, 13, 14, 14, 15, 15,
    15, 16, 16, 16, 17, 17, 17, 18, 18, 19, 19, 19, 20, 20, 20, 21, 21,
    22, 22, 22, 23, 23, 23, 24, 24, 24, 25, 25, 26, 26, 26, 27, 27, 27,
    28, 28, 29, 29, 29, 30, 30, 30, 31, 31, 31, 32, 32, 33, 33, 33, 34,
    34, 34, 35, 35, 36, 36, 36, 37, 37, 37, 38, 38, 38, 39, 39, 40, 40,
    40, 41, 41, 41, 42, 42, 43, 43, 43, 44, 44, 44, 45, 45, 46, 46, 46,
    47, 47, 47, 48, 48, 48, 49, 49, 50, 50, 50, 51, 51, 52, 52, 52, 53,
    53, 53, 54, 54, 54, 55, 55, 56, 56, 56, 56, 57, 57, 57, 58, 58, 59,
    59, 59, 59, 60, 60, 61, 61, 62, 62, 63, 63, 64, 64, 64, 64, 65, 65,
    65, 65, 66, 66, 66, 67, 67, 68, 68, 69, 69, 69, 69, 70, 71, 71, 71,
    71, 71, 71, 72, 72, 73, 73, 74, 74, 74, 75, 75, 75, 75, 76, 76, 77,
    77, 77, 77, 78, 79, 79, 79, 79, 79, 80, 80, 80, 81, 82, 82, 82, 83,
    83, 84, 84, 84, 85, 85, 85, 86, 86, 86, 86, 87, 87, 87, 87, 88, 88,
    89, 89, 90, 90, 91, 91, 91, 92, 92, 93, 93, 94, 94, 94, 94, 95, 95,
    96, 96, 96, 96, 97, 97, 98, 98, 98, 99, 99, 100, 100, 100, 101, 101,
    101, 102, 102, 102, 103, 103, 104, 104, 104, 105, 105, 105, 106, 106,
    106, 107, 107, 107, 108, 108, 109, 109, 109, 110, 110, 111, 111, 111,
    112, 112, 112, 113, 113, 114, 114, 114, 115, 115, 116, 116, 116, 117,
    117, 117, 117, 118, 118, 119, 119, 119, 120, 120, 121, 121, 121, 122,
    122, 122, 123, 123, 124, 124, 124, 124, 125, 125, 126, 126, 126, 126,
    127, 127, 128, 128, 129, 129, 130, 130, 130, 130, 131, 131, 132, 132,
    132, 133, 133, 133, 134, 134, 135, 135, 135, 136, 136, 136, 137, 137,
    137, 138, 138, 139, 139, 139, 140, 140, 141, 141, 141, 142, 142, 142,
    143, 143, 143, 144, 144, 144, 145, 145, 146, 146, 146, 147, 147, 147,
    148, 148, 149, 149, 149, 150, 150, 150, 151, 151, 151, 152, 152, 153,
    153, 153, 154, 154, 154, 155, 155, 156, 156, 156, 157, 157, 157, 158,
    158, 158, 159, 159, 160, 160, 160, 161, 161, 161, 162, 162, 163, 163,
    163, 164, 164, 164, 165, 165, 165, 166, 166, 167, 167, 167, 168, 168,
    168, 169, 169, 170, 170, 170, 171, 171, 171, 172, 172, 172, 173, 173,
    174, 174, 174, 175, 175, 175, 176, 176, 177, 177, 177, 178, 178, 178,
    179, 179, 179, 180, 180, 181, 181, 181, 182, 182, 182, 183, 183, 184,
    184, 184, 185, 185, 185, 186, 186, 186, 187, 187, 188, 188, 188, 189,
    189, 189, 190, 190, 191, 191, 191, 192, 192, 192, 193, 193, 193, 194,
    194, 195, 195, 195, 196, 196, 196, 197, 197, 198, 198, 198, 199, 199,
    199, 200, 200, 200, 201, 201, 202, 202, 202, 203, 203, 203, 204, 204,
    205, 205, 205, 206, 206, 206, 207, 207, 207, 208, 208, 209, 209, 209,
    210, 210, 210, 211, 211, 212, 212, 212, 213, 213, 213, 214, 214, 214,
    214, 214, 215, 215, 215, 216, 216, 216, 217, 217, 218, 218, 218, 219,
    219, 219, 220, 220, 221, 221, 221, 222, 222, 222, 223, 223, 223, 224,
    224, 225, 225, 225, 226, 226, 226, 227, 227, 228, 228, 228, 229, 229,
    229, 230, 230, 230, 231, 231, 232, 232, 232, 233, 233, 233, 234, 234,
    235, 235, 235, 236, 236, 236, 237, 237, 237, 238, 238, 239, 239, 239,
    240, 240, 240, 241, 241, 242, 242, 242, 243, 243, 243, 244, 244, 244,
    245, 245, 246, 246, 246, 247, 247, 247, 248, 248, 249, 249, 249, 250,
    250, 250, 251, 251, 252, 252, 252, 253, 253, 253, 254, 254, 254, 255,
    255, 256, 256, 256, 257, 257, 257, 258, 258, 259, 259, 259, 260, 260,
    260, 261, 261, 261, 262, 262, 263, 263, 263, 264, 264, 264, 265, 265,
    266, 266, 266, 267, 267, 267, 268, 268, 268, 269, 269, 270, 270, 270,
    271, 271, 271, 272, 272, 273, 273, 273, 274, 274, 274, 275, 275, 276,
    276, 276, 277, 277, 277, 278, 278, 278, 279, 279, 280, 280, 280, 281,
    281, 281, 282, 282, 283, 283, 283, 284, 284, 284, 285, 285, 285, 286,
    286, 287, 287, 287, 288, 288, 288, 289, 289, 290, 290, 290, 291, 291,
    291, 292, 292, 292, 293, 293, 294, 294, 294, 295, 295, 295, 296, 296,
    297, 297, 297, 298, 298, 298, 299, 299, 299, 300, 300, 301, 301, 301,
    302, 302, 302, 303, 303, 304, 304, 304, 305, 305, 305, 306, 306, 306,
    307, 307, 308, 308, 308, 309, 309, 309, 310, 310, 311, 311, {����ǹ�Ԫǰ 1 ��}
    {�ұ��ǹ�ԪԪ��} 312, 312,
    312, 313, 313, 313, 314, 314, 315, 315, 315, 316, 316, 316, 317, 317,
    317, 318, 318, 319, 319, 319, 320, 320, 320, 321, 321, 322, 322, 322,
    323, 323, 323, 324, 324, 325, 325, 325, 326, 326, 326, 327, 327, 327,
    328, 328, 329, 329, 329, 330, 330, 330, 331, 331, 332, 332, 332, 333,
    333, 333, 334, 334, 334, 335, 335, 336, 336, 336, 337, 337, 337, 338,
    338, 339, 339, 339, 340, 340, 340, 341, 341, 341, 342, 342, 343, 343,
    343, 344, 344, 344, 345, 345, 346, 346, 346, 347, 347, 347, 348, 348,
    348, 349, 349, 350, 350, 350, 351, 351, 351, 352, 352, 353, 353, 353,
    354, 354, 354, 355, 355, 355, 356, 356, 357, 357, 357, 358, 358, 358,
    359, 359, 360, 360, 360, 361, 361, 361, 362, 362, 362, 363, 363, 364,
    364, 364, 365, 365, 365, 366, 366, 367, 367, 367, 368, 368, 368, 369,
    369, 369, 370, 370, 371, 371, 371, 372, 372, 372, 373, 373, 374, 374,
    374, 375, 375, 375, 376, 376, 376, 377, 377, 378, 378, 378, 379, 379,
    379, 380, 380, 381, 381, 381, 382, 382, 382, 383, 383, 383, 384, 384,
    385, 385, 385, 386, 386, 386, 387, 387, 388, 388, 388, 389, 389, 389,
    390, 390, 390, 391, 391, 392, 392, 392, 393, 393, 393, 394, 394, 395,
    395, 395, 396, 396, 396, 397, 397, 397, 398, 398, 399, 399, 399, 400,
    400, 400, 401, 401, 402, 402, 402, 403, 403, 403, 404, 404, 404, 405,
    405, 406, 406, 406, 407, 407, 407, 408, 408, 409, 409, 409, 410, 410,
    410, 411, 411, 411, 412, 412, 413, 413, 413, 414, 414, 414, 415, 415,
    416, 416, 416, 417, 417, 417, 418, 418, 418, 419, 419, 420, 420, 420,
    421, 421, 421, 422, 422, 423, 423, 423, 424, 424, 424, 425, 425, 425,
    426, 426, 427, 427, 427, 428, 428, 428, 429, 429, 430, 430, 430, 431,
    431, 431, 432, 432, 432, 433, 433, 434, 434, 434, 435, 435, 435, 436,
    436, 437, 437, 437, 438, 438, 438, 439, 439, 439, 440, 440, 441, 441,
    441, 442, 442, 442, 443, 443, 444, 444, 444, 445, 445, 445, 446, 446,
    446, 447, 447, 448, 448, 448, 449, 449, 449, 450, 450, 451, 451, 451,
    452, 452, 452, 453, 453, 453, 454, 454, 455, 455, 455, 456, 456, 456,
    457, 457, 458, 458, 458, 459, 459, 459, 460, 460, 460, 461, 461, 462,
    462, 462, 463, 463, 463, 464, 464, 465, 465, 465, 466, 466, 466, 467,
    467, 467, 468, 468, 469, 469, 469, 470, 470, 470, 471, 471, 472, 472,
    472, 473, 473, 473, 474, 474, 474, 475, 475, 475, 476, 476, 477, 477,
    477, 478, 478, 478, 479, 479, 480, 480, 480, 481, 481, 481, 482, 482,
    482, 483, 483, 484, 484, 484, 485, 485, 485, 486, 486, 487, 487, 487,
    488, 488, 488, 489, 489, 489, 490, 490, 491, 491, 491, 492, 492, 492,
    493, 493, 494, 494, 494, 495, 495, 495, 496, 496, 496, 497, 497, 498,
    498, 498, 499, 499, 499, 500, 500, 501, 501, 501, 502, 502, 502, 503,
    503, 503, 504, 504, 505, 505, 505, 506, 506, 506, 507, 507, 508, 508,
    508, 509, 509, 509, 510, 510, 510, 511, 511, 512, 512, 512, 513, 513,
    513, 514, 514, 515, 515, 515, 516, 516, 516, 517, 517, 517, 518, 518,
    519, 519, 519, 520, 520, 520, 521, 521, 522, 522, 522, 523, 523, 523,
    524, 524, 524, 525, 525, 526, 526, 526, 527, 527, 527, 528, 528, 529,
    529, 529, 530, 530, 530, 531, 531, 531, 532, 532, 533, 533, 533, 534,
    534, 534, 535, 535, 536, 536, 536, 537, 537, 537, 538, 538, 538, 539,
    539, 540, 540, 540, 541, 541, 541, 542, 542, 543, 543, 543, 544, 544,
    544, 545, 545, 545, 546, 546, 547, 547, 547, 548, 548, 548, 549, 549,
    550, 550, 550, 551, 551, 551, 552, 552, 552, 553, 553, 554, 554, 554,
    555, 555, 555, 556, 556, 557, 557, 557, 558, 558, 558, 559, 559, 559,
    560, 560, 561, 561, 561, 562, 562, 562, 563, 563, 563, 564, 564, 565,
    565, 565, 566, 566, 566, 567, 567, 568, 568, 568, 569, 569, 569, 570,
    570, 570, 571, 571, 572, 572, 572, 573, 573, 573, 574, 574, 575, 575,
    575, 576, 576, 576, 577, 577, 578, 578, 578, 579, 579, 579, 580, 580,
    580, 581, 581, 582, 582, 582, 583, 583, 583, 584, 584, 584, 585, 585,
    586, 586, 586, 587, 587, 587, 588, 588, 589, 589, 589, 590, 590, 590,
    591, 591, 591, 592, 592, 593, 593, 593, 594, 594, 594, 595, 595, 596,
    596, 596, 597, 597, 597, 598, 598, 598, 599, 599, 600, 600, 600, 601,
    601, 601, 602, 602, 603, 603, 603, 604, 604, 604, 605, 605, 605, 606,
    606, 607, 607, 607, 608, 608, 608, 609, 609, 610, 610, 610, 611, 611,
    611, 612, 612, 612, 613, 613, 614, 614, 614, 615, 615, 615, 616, 616,
    617, 617, 617, 618, 618, 618, 619, 619, 619, 620, 620, 621, 621, 621,
    622, 622, 622, 623, 623, 624, 624, 624, 625, 625, 625, 626, 626, 626,
    627, 627, 628, 628, 628, 629, 629, 629, 630, 630, 631, 631, 631, 632,
    632, 632, 633, 633, 633, 634, 634, 635, 635, 635, 636, 636, 636, 637,
    637, 638, 638, 638, 639, 639, 639, 640, 640, 640, 641, 641, 642, 642,
    642, 643, 643, 643, 644, 644, 645, 645, 645, 646, 646, 646, 647, 647,
    647, 648, 648, 649, 649, 649, 650, 650, 650, 651, 651, 652, 652, 652,
    653, 653, 653, 654, 654, 654, 655, 655, 656, 656, 656, 657, 657, 657,
    658, 658, 659, 659, 659, 660, 660, 660, 661, 661, 661, 662, 662, 663,
    663, 663, 664, 664, 664, 665, 665, 666, 666, 666, 667, 667, 667, 668,
    668, 668, 669, 669, 670, 670, 670, 671, 671, 671, 672, 672, 673, 673,
    673, 674, 674, 674, 675, 675, 675, 676, 676, 677, 677, 677, 678, 678,
    678, 679, 679, {��Ԫ 999 ���� 1000 ��ֽ�} 680, {��߹�Ԫ 1000 �꣬�ұ߹�Ԫ 1001 ��}
    680, 680, 681, 681, 681, 682, 682, 682, 683, 683,
    684, 684, 684, 685, 685, 685, 686, 686, 687, 687, 687, 688, 688, 688,
    689, 689, 689, 690, 690, 691, 691, 691, 692, 692, 692, 693, 693, 694,
    694, 694, 695, 695, 695, 696, 696, 696, 697, 697, 698, 698, 698, 699,
    699, 699, 700, 700, 701, 701, 701, 702, 702, 702, 703, 703, 703, 704,
    704, 705, 705, 705, 706, 706, 706, 707, 707, 707, 708, 708, 709, 709,
    709, 710, 710, 710, 711, 711, 712, 712, 712, 713, 713, 713, 714, 714,
    714, 715, 715, 716, 716, 716, 717, 717, 717, 718, 718, 719, 719, 719,
    720, 720, 720, 721, 721, 721, 722, 722, 723, 723, 723, 724, 724, 724,
    725, 725, 726, 726, 726, 727, 727, 727, 728, 728, 728, 729, 729, 730,
    730, 730, 731, 731, 731, 732, 732, 733, 733, 733, 734, 734, 734, 735,
    735, 736, 736, 736, 737, 737, 737, 738, 738, 738, 739, 739, 740, 740,
    740, 741, 741, 741, 742, 742, 742, 743, 743, 744, 744, 744, 745, 745,
    745, 746, 746, 747, 747, 747, 748, 748, 748, 749, 749, 749, 750, 750,
    751, 751, 751, 752, 752, 752,
    {�˴� 1199 �� 1200 ���� 2 ���䣬��ԭʼ���ݴ��󣬿�ʼ����һ}
    753, 753, 754, 754, 754, 755, 755, 755,
    756, 756, 756, 757, 757, 758, 758, 758, 759, 759, 759, 760, 760, 761,
    761, 761, 762, 762, 762, 763, 763, 763, 764, 764, 765, 765, 765, 766,
    766, 766, 767, 767, 768, 768, 768, 769, 769, 769, 770, 770, 770, 771,
    771, 772, 772, 772, 773, 773, 773, 774, 774, 775, 775, 775, 776, 776,
    776, 777, 777, 777, 778, 778, 779, 779, 779, 780, 780, 780, 781, 781,
    782, 782, 782, 783, 783, 783, 784, 784, 784, 785, 785, 786, 786, 786,
    787, 787, 787, 788, 788, 789, 789, 789, 790, 790, 790, 791, 791, 791,
    792, 792, 793, 793, 793, 794, 794, 794, 795, 795, 795, 796, 796, 797,
    797, 797, 798, 798, 798, 799, 799, 800, 800, 800, 801, 801, 801, 802,
    802, 803, 803, 803, 804, 804, 804, 805, 805, 805, 806, 806, 807, 807,
    807, 808, 808, 808, 809, 809, 809, 810, 810, 811, 811, 811, 812, 812,
    812, 813, 813, 814, 814, 814, 815, 815, 815, 816, 816, 817, 817, 817,
    818, 818, 818, 819, 819, 819, 820, 820, 821, 821, 821, 822, 822, 822,
    823, 823, 824, 824, 824, 825, 825, 825, 826, 826, 826, 827, 827, 828,
    828, 828, 829, 829, 829, 830, 830, 831, 831, 831, 832, 832, 832, 833,
    833, 833, 834, 834, 835, 835, 835, 836, 836, 836, 837, 837, 838, 838,
    838, 839, 839, 839, 840, 840, 840, 841, 841, 842, 842, 842, 843, 843,
    843, 844, 844, 844, 845, 845, 846, 846, 846, 847, 847, 847, 848, 848,
    849, 849, 849, 850, 850, 850, 851, 851, 851, 852, 852, 853, 853, 853,
    854, 854, 854, 855, 855, 856, 856, 856, 857, 857, 857, 858, 858, 858,
    859, 859, 860, 860, 860, 861, 861, 861, 862, 862, 863, 863,
    {�˴� 1499 �� 1500 �����������䣬��ԭʼ���ݴ��󣬽�����һ} 863, 864,
    864, 864, 865, 865, 865, 866, 866, 867, 867, 867, 868, 868, 868, 869,
    869, 870, 870, 870, 871, 871, 871, 872, 872, 873, 873, 873, 874, 874,
    874, 875, 875, 875, 876, 876, 877, 877, 877, 878, 878, 878, 879, 879,
    879, 880, 880, 881, 881, 881, 882, 882, 882, 883, 883, 884, 884, 884,
    885, 885, 885, 886, 886, 886, 887, 887, 888, 888, 888, 889, 889, 889,
    890, 890, 891, 891, 891, 892, 892, 892, 893, 893, 893, 894, 894, 895,
    895, 895, 896, 896, 896, 897, 897, 898, 898, 898, 899, 899, 899, 900,
    900, 900, 901, 901, 902, 902, 902, 903, 903, 903, 904, 904, 905, 905,
    905, 906, 906, 906, 907, 907, 907, 908, 908, 909, 909, 909, 910, 910,
    910, 911, 911, 912, 912, 912, 913, 913, 913, 914, 914, 914, 915, 915,
    916, 916, 916, 917, 917, 917, 918, 918, 919, 919, 919, 920, 920, 920,
    921, 921, 921, 922, 922, 923, 923, 923, 924, 924, 924, 925, 925, 925,
    926, 926, 927, 927, 927, 928, 928, 928, 929, 929, 930, 930, 930, 931,
    931, 931, 932, 932, 932, 933, 933, 934, 934, 934, 935, 935, 935, 936,
    936, 937, 937, 937, 938, 938, 938, 939, 939, 939, 940, 940, 941, 941,
    941, 942, 942, 942, 943, 943, 944, 944, 944, 945, 945, 945, 946, 946,
    946, 947, 947, 948, 948, 948, 949, 949, 949, 950, 950, 951, 951, 951,
    952, 952, 952, 953, 953, 953, 954, 954, 955, 955, 955, 956, 956, 956,
    957, 957, 958, 958, 958, 959, 959, 959, 960, 960, 960, 961, 961, 962,
    962, 962, 963, 963, 963, 964, 964, 965, 965, 965, 966, 966, 966, 967,
    967, 967, 968, 968, 969, 969, 969, 970, 970, 970, 971, 971, 971, 972,
    972, 973, 973, 973, 974, 974, 974, 975, 975, 976, 976, 976, 977, 977,
    977, 978, 978, 978, 979, 979, 980, 980, 980, 981, 981, 981, 982, 982,
    983, 983, 983, 984, 984, 984, 985, 985, 986, 986, 986, 987, 987, 987,
    988, 988, 988, 989, 989, 990, 990, 990, 991, 991, 991, 992, 992, 993,
    993, 993, 994, 994, 994, 995, 995, 995, 996, 996, 997, 997, 997, 998,
    998, 998, 999, 999, 1000, 1000, 1000, 1001, 1001, 1001, 1002, 1002,
    1002, 1003, 1003, 1004, 1004, 1004, 1005, 1005, 1005, 1006, 1006,
    1006, 1007, 1007, 1008, 1008, 1008, 1009, 1009, 1009, 1010, 1010,
    1011, 1011, 1011, 1012, 1012, 1012, 1013, 1013, 1013, 1014, 1014,
    1015, 1015, 1015, 1016, 1016, 1016, 1017, 1017, 1018, 1018, 1018,
    1019, 1019, 1019, 1020, 1020, 1020, 1021, 1021, 1022, 1022, 1022,
    1023, 1023, 1023, 1024, 1024, 1025, 1025, 1025, 1026, 1026, 1026,
    1027, 1027, 1027, 1028, 1028, 1029, 1029, 1029, 1030, 1030, 1030,
    1031, 1031, 1032, 1032, 1032, 1033, 1033, 1033, 1034, 1034, 1034,
    1035, 1035, 1036, 1036, 1036, 1037, 1037, 1037, 1038, 1038, 1039,
    1039, 1039, 1040, 1040, 1040, 1041, 1041, 1042, 1042, 1042, 1043,
    1043, 1043, 1044, 1044, 1044, 1045, 1045, 1046, 1046, 1046, 1047,
    1047, 1047, {��Ԫ 1999 ���� 2000 ��ֽ�} 1048, {��߹�Ԫ 2000 �꣬�ұ߹�Ԫ 2001 ��}
    1048, 1048, 1049, 1049, 1050, 1050, 1050, 1051,
    1051, 1051, 1052, 1052, 1053, 1053, 1053, 1054, 1054, 1054, 1055,
    1055, 1055, 1056, 1056, 1057, 1057, 1057, 1058, 1058, 1058, 1059,
    1059, 1060, 1060, 1060, 1061, 1061, 1061, 1062, 1062, 1062, 1063,
    1063, 1064, 1064, 1064, 1065, 1065, 1065, 1066, 1066, 1067, 1067,
    1067, 1068, 1068, 1068, 1069, 1069, 1069, 1070, 1070, 1071, 1071,
    1071, 1072, 1072, 1072, 1073, 1073, 1074, 1074, 1074, 1075, 1075,
    1075, 1076, 1076, 1076, 1077, 1077, 1078, 1078, 1078, 1079, 1079,
    1079, 1080, 1080, 1081, 1081, 1081, 1082, 1082, 1082, 1083, 1083,
    1083, 1084, 1084, {��Ԫ 2099 ���� 2100 ��ֽ���}
    1085, 1085, 1085, 1086, 1086, 1086, 1087, 1087, 1088, 1088, 1088,
    1089, 1089, 1089, 1090, 1090, 1090, 1091, 1091, 1092, 1092, 1092,
    1093, 1093, 1093, 1094, 1094, 1095, 1095, 1095, 1096, 1096, 1096,
    1097, 1097, 1097, 1098, 1098, 1099, 1099, 1099, 1100, 1100, 1100,
    1101, 1101, 1102, 1102, 1102, 1103, 1103, 1103, 1104, 1104, 1104,
    1105, 1105, 1106, 1106, 1106, 1107, 1107, 1107, 1108, 1108, 1109,
    1109, 1109, 1110, 1110, 1110, 1111, 1111, 1111, 1112, 1112, 1113,
    1113, 1113, 1114, 1114, 1114, 1115, 1115, 1115, 1116, 1116, 1117,
    1117, 1117, 1118, 1118, 1118, 1119, 1119, 1120, 1120, 1120, 1121,
    1121, 1121, 1122, 1122, 1123, 1123, 1123, 1124, 1124, 1124, 1125,
    1125, 1125, 1126, 1126, 1127, 1127, 1127, 1128, 1128, 1128, 1129,
    1129, 1130, 1130, 1130, 1131, 1131, 1131, 1132, 1132, 1132, 1133,
    1133, 1134, 1134, 1134, 1135, 1135, 1135, 1136, 1136, 1137, 1137,
    1137, 1138, 1138, 1138, 1139, 1139, 1139, 1140, 1140, 1141, 1141,
    1141, 1142, 1142, 1142, 1143, 1143, 1143, 1144, 1144, 1145, 1145,
    1145, 1146, 1146, 1146, 1147, 1147, 1148, 1148, 1148, 1149, 1149,
    1149, 1150, 1150, 1150, 1151, 1151, 1152, 1152, 1152, 1153, 1153,
    1153, 1154, 1154, 1155, 1155, 1155, 1156, 1156, 1156, 1157, 1157,
    1157, 1158, 1158, 1159, 1159, 1159, 1160, 1160, 1160, 1161, 1161,
    1162, 1162, 1162, 1163, 1163, 1163, 1164, 1164, 1165, 1165, 1165,
    1166, 1166, 1166, 1167, 1167, 1167, 1168, 1168, 1169, 1169, 1169,
    1170, 1170, 1170, 1171, 1171, 1171, 1172, 1172, 1173, 1173, 1173,
    1174, 1174, 1174, 1175, 1175, 1176, 1176, 1176, 1177, 1177, 1177,
    1178, 1178, 1178, 1179, 1179, 1180, 1180, 1180, 1181, 1181, 1181,
    1182, 1182, 1183, 1183, 1183, 1184, 1184, 1184, 1185, 1185, 1185,
    1186, 1186, 1187, 1187, 1187, 1188, 1188, 1188, 1189, 1189, 1190,
    1190, 1190, 1191, 1191, 1191, 1192, 1192, 1192, 1193, 1193, 1194,
    1194, 1194, 1195, 1195, 1195, 1196, 1196, 1197, 1197, 1197, 1198,
    1198, 1198, 1199, 1199, 1199, 1200, 1200, 1201, 1201, 1201, 1202,
    1202, 1202, 1203, 1203, 1204, 1204, 1204, 1205, 1205, 1205, 1206,
    1206, 1206, 1207, 1207, 1208, 1208, 1208, 1209, 1209, 1209, 1210,
    1210, 1211, 1211, 1211, 1212, 1212, 1212, 1213, 1213, 1213, 1214,
    1214, 1215, 1215, 1215, 1216, 1216, 1216, 1217, 1217, 1218, 1218,
    1218, 1219, 1219, 1219, 1220, 1220, 1220, 1221, 1221, 1222, 1222,
    1222, 1223, 1223, 1223, 1224, 1224, 1225, 1225, 1225, 1226, 1226,
    1226, 1227, 1227, 1227, 1228, 1228, 1229, 1229, 1229, 1230, 1230,
    1230, 1231, 1231, 1232, 1232, 1232, 1233, 1233, 1233, 1234, 1234,
    1234, 1235, 1235, 1236, 1236, 1236, 1237, 1237, 1237, 1238, 1238,
    1238, 1239, 1239, 1240, 1240, 1240, 1241, 1241, 1241, 1242, 1242,
    1243, 1243, 1243, 1244, 1244, 1244, 1245, 1245, 1245, 1246, 1246,
    1247, 1247, 1247, 1248, 1248, 1248, 1249, 1249, 1250, 1250, 1250,
    1251, 1251, 1251, 1252, 1252, 1253, 1253, 1253, 1254, 1254, 1254,
    1255, 1255, 1255, 1256, 1256, 1257, 1257, 1257, 1258, 1258, 1258,
    1259, 1259, 1260, 1260, 1260, 1261, 1261, 1261, 1262, 1262, 1262,
    1263, 1263, 1264, 1264, 1264, 1265, 1265, 1265, 1266, 1266, 1267,
    1267, 1267, 1268, 1268, 1268, 1269, 1269, 1269, 1270, 1270, 1271,
    1271, 1271, 1272, 1272, 1272, 1273, 1273, 1274, 1274, 1274, 1275,
    1275, 1275, 1276, 1276, 1276, 1277, 1277, 1278, 1278, 1278, 1279,
    1279, 1279, 1280, 1280, 1280, 1281, 1281, 1282, 1282, 1282, 1283,
    1283, 1283, 1284, 1284, 1285, 1285, 1285, 1286, 1286, 1286, 1287,
    1287, 1287, 1288, 1288, 1289, 1289, 1289, 1290, 1290, 1290, 1291,
    1291, 1292, 1292, 1292, 1293, 1293, 1293, 1294, 1294, 1294, 1295,
    1295, 1296, 1296, 1296, 1297, 1297, 1297, 1298, 1298, 1299, 1299,
    1299, 1300, 1300, 1300, 1301, 1301, 1301, 1302, 1302, 1303, 1303,
    1303, 1304, 1304, 1304, 1305, 1305, 1306, 1306, 1306, 1307, 1307,
    1307, 1308, 1308, 1308, 1309, 1309, 1310, 1310, 1310, 1311, 1311,
    1311, 1312, 1312, 1313, 1313, 1313, 1314, 1314, 1314, 1315, 1315,
    1316, 1316, 1316, 1317, 1317, 1317, 1318, 1318, 1318, 1319, 1319,
    1320, 1320, 1320, 1321, 1321, 1321, 1322, 1322, 1322, 1323, 1323,
    1324, 1324, 1324, 1325, 1325, 1325, 1326, 1326, 1327, 1327, 1327,
    1328, 1328, 1328, 1329, 1329, 1329, 1330, 1330, 1331, 1331, 1331,
    1332, 1332, 1332, 1333, 1333, 1334, 1334, 1334, 1335, 1335, 1335,
    1336, 1336, 1336, 1337, 1337, 1338, 1338, 1338, 1339, 1339, 1339,
    1340, 1340, 1341, 1341, 1341, 1342, 1342 {����ǹ�Ԫ 2799 ��}
  );
  { * �Թ�Ԫǰ 850 �꿪ʼ��ũ����������-849~2100 ��ֲ���й������࣬2100 ���޽��ʼ��㲹��
    0~3648 �� 3649 �������Ԫǰ 850 �굽��Ԫǰһ��� 850 ���ԪԪ�굽��Ԫ 2799 ��� 2799 �
    �޲����ڵĹ�Ԫ 0 ��}

  SCnLeapMonth =
    '0c0080050010a0070030c0080050010a0070030c0080050020a0070030c0080050020a' +
    '0070030c0090050020a0070030c0090050020a0060030c0060030c00900600c0c0060c' +
    '00c00c00c0c000600c0c0006090303030006000c00c060c0006c00000c0c0c00600030' +
    '30006c00009009c0090c00c009000300030906030030c0c00060c00090c0060600c003' +
    '0060c00c003006009060030c0060060c0090900c00090c0090c00c0060300060600030' +
    '30c0c00030c0060030c0090060030c0090300c0080050020a0060030c0080050020b00' +
    '70030c0090050010a0070030b0090060020a0070040c0080050020a0060030c0080050' +
    '020b0070030c0090050010a0070030b0090060020a0070040c0080050020a0060030c0' +
    '080050020b0070030c0090050000c00900909009009090090090090900900909009009' +
    '0090900900909009009009090090090900900900909009009090090090900900900909' +
    '00900909009009009090090090900900900909009009090060030c0090050010a00700' +
    '30b008005001090070040c0080050020a0060030c0090040010a0060030c0090050010' +
    'a0070030b0' + '0' + '80050010a008005001090050020a0060030c0080040010a0060030c0090' +
    // ���� 70030b008005 �У�b008 �ĵڶ��� 0 �ǹ�Ԫ 0 ��ķǷ����ݣ�����ռλ��
    '050010a0070030b0080050010a0070030b008005001090070040c0080050020a006003' +
    '0c0080040010a0060030c0090050010a0070030b008005001090070040c0080050020a' +
    '0060030c0080040010a0060030c0090050010a0060030c0090050010a0070030b00800' +
    '5001090070040c0080050020a0060030c0080040010a0070030b0080050010a0070040' +
    'c0080050020a0060030c0080040010a0070030c0090050010a0070030b0080050020a0' +
    '060030c0080040010a0060030c0090050050020a0060030c0090050010b0070030c009' +
    '0050010a0070040c0080040020a0060030c0080050020a0060030c0090050010a00700' +
    '30b0080040020a0060040c0090050020b0070030c00a0050010a0070030b0090050020' +
    'a0070030c0080040020a0060030c0090050010a0070030c0090050030b007005001090' +
    '050020a007004001090060020c0070050c0090060030b0080040020a0060030b008004' +
    '0010a0060030b0080050010a0050040c0080050010a0060030c0080050010b0070030c' +
    '007005001090070030b0070040020a0060030c0080040020a0070030b0090050010a00' +
    '60040c0080050020a0060040c0080050010b0070030c007005001090070030c0080050' +
    '020a0070030c0090050020a0070030c0090050020a0060040c0090050020a0060040c0' +
    '090050010b0070030c0080050030b007004001090060020c008004002090060020a008' +
    '004001090050030b0080040020a0060040b0080040c00a0060020b0070050010900600' +
    '30b0070050020a0060020c008004002090070030c008005002090070040c0080040020' +
    'a0060040b0090050010a0060030b0080050020a0060040c0080050010b007003001080' +
    '05001090070030c0080050020a007003001090050030a0070030b0090050020a006004' +
    '0c0090050030b0070040c0090050010c0070040c0080060020b00700400a090060020b' +
    '007003002090060020a005004001090050030b007004001090050040c0080040c00a00' +
    '60020c007005001090060030b0070050020a0060020c008004002090060030b0080040' +
    '02090060030b0080040020a0060040b0080040010b0060030b0070050030a006004002' +
    '0700500308006004003070050030700600400307005003080060040030700500409006' +
    '0040030700500409006005002070050030a00600500307005004002060040020600500' +
    '30020600400307005004090060040030700500408007005003080050040a0060050030' +
    '7005004002060050030800500400206005002070050040020600500307006004002070' +
    '050030800600400307005004080060040a006005003080050040020700500409006004' +
    '002060050030b006005002070050030800600400307005004080060040030700500408' +
    '0060040020' +
    '700500409006004003070050040b006005002070050040b006005003070060040a0060' +
    '0500307006004002060050030700600409006004003070050040900700500308005004' +
    '0b00600500307006005001070050030800600400206005003070060040020600500307' +
    '0060040a00700500308006004003070050040800600500107005004080060050020700' +
    '50040a0060040020600500308006005002070050030800600400307005004080070050' +
    '030800500408006005003070050040a006005003070050040a00600500207005004001' +
    '0600500307006004001070050030700600408007005004070060040900600400307005'+
    '0040a007005003080060040b0060050030800600500107005003080060040020700500' +
    '3070060040030700500307006004003070050030800600400307005004090060050b00' +
    '7005004090060050020700600408006005003070060030800600500307006003080060'
    ;
  { * �Թ�Ԫǰ 850 �꿪ʼ��ũ��������Ϣ -849~2100����ֲ���й������࣬2100 ���޽��ʼ��㲹��
    �� 3650 ���Ȼ������Ķ�һ�ԭ����Ϊ�˷���ֱ�Ӱ���Ԫ��� +849 ���±���ʣ�
    �ڲ������˸���Ԫ 0 ��� 0 ֵ���ѽ⵫Ŀ�ⲻӰ��}

type
  TCnLunarDateSingleMonthFix = packed record
  {* ��Ŵ��۲⾫�����ƣ���Ե���ũ���µĹ�ũ��ת������һ������ݣ���ʱû�����������}
    Year: Integer;         // ƫ�����ڵ����
    Month: Integer;        // ƫ�ʼ���·ݣ���������һ����
    StartDay: Integer;     // ƫ�ʼ������
    EndDay: Integer;       // �¸���ƫ�����������
    IncOne: Boolean;       // True ��ʾ��һ�죬False ��ʾ��һ��
  end;

const
  CN_LUNAR_SINGLE_MONTH_FIX: array[0..8] of TCnLunarDateSingleMonthFix = (
  {* ��ʷ����۲�ƫ��µĵ���ũ�����׵ĵ���ƫ������}
    (Year:  244; Month:  4; StartDay: 24; EndDay: 23; IncOne: False),
    (Year:  245; Month:  1; StartDay: 15; EndDay: 13; IncOne: True),
    (Year:  245; Month:  5; StartDay: 13; EndDay: 11; IncOne: False),
    (Year:  245; Month:  7; StartDay: 11; EndDay:  9; IncOne: False),
    (Year:  246; Month:  2; StartDay:  3; EndDay:  4; IncOne: True),
    (Year: 1199; Month:  3; StartDay: 28; EndDay: 26; IncOne: False),
    (Year: 1914; Month: 11; StartDay: 17; EndDay: 16; IncOne: True),
    (Year: 1924; Month:  3; StartDay:  5; EndDay:  3; IncOne: True),
    (Year: 2018; Month: 11; StartDay:  7; EndDay:  6; IncOne: False)
  );

// �޹�ԪԪ��Ĺ�����ݣ�ת��Ϊ�ڲ������İ��� 0 ����ݣ���ֵ��һ
procedure NonZeroYearToZeroYear(var AYear: Integer);
begin
  if AYear = 0 then
    raise ECnDateTimeException.Create(SCnErrorYearIsInvalid);

  if AYear < 0 then
    Inc(AYear);
end;

// �ڲ������İ��� 0 ����ݣ�ת��Ϊ�޹�ԪԪ��Ĺ�����ݣ�����ֵ��һ
procedure ZeroYearToNonZeroYear(var AYear: Integer);
begin
  if AYear <= 0 then
    Dec(AYear);
end;

//==============================================================================
// �������ճ�������������
//==============================================================================

function HoursMin(Hours: Extended): TDateTime;
begin
  Result := Hours / 24;
end;

function IntPart(X: Extended): Extended;
begin
  if X > 0 then
    Result := Floor(X)
  else
    Result := Ceil(X);
end;

function Range360(X: Extended): Extended;
var
  A: Extended;
begin
  A := X / 360;
  Result := 360 * (A - IntPart(A));
  if Result < 0 then
    Result := Result + 360;
end;

// �ճ�����ר�õļ���Լ��������
function Mjd(Year, Month, Day, Hour: Integer): Extended;
var
  A, B: Extended;
begin
  if Month <= 2 then
  begin
    Month := Month + 12;
    Year := Year - 1;
  end;

  A := 10000.0 * Year + 100.0 * Month + Day;
  if A <= 15821004.1 then
  begin
    B := -2 * Floor((Year + 4716) / 4) - 1179;
  end
  else
  begin
    B := Floor(Year / 400) - Floor(Year / 100) + Floor(Year / 4);
  end;

  A := 365.0 * Year - 679004.0;
  Result := A + B + Floor(30.6001 * (Month + 1)) + Day + Hour / 24.0;
end;

procedure Quad(ym, yz, yp: Extended; var nz, z1, z2, xe, ye: Extended);
var
  A, B, c, dis, dx: Extended;
begin
  nz := 0;
  A := 0.5 * (ym + yp) - yz;
  B := 0.5 * (yp - ym);
  c := yz;
  xe := -B / (2 * A);
  ye := (A * xe + B) * xe + c;
  dis := B * B - 4.0 * A * c;
  if dis > 0 then
  begin
    dx := 0.5 * Sqrt(dis) / Abs(A);
    z1 := xe - dx;
    z2 := xe + dx;
    if Abs(z1) <= 1.0 then
      nz := nz + 1;
    if Abs(z2) <= 1.0 then
      nz := nz + 1;
    if z1 < -1.0 then
      z1 := z2;
  end;
end;

function Lmst(AMjd, Glong: Extended): Extended;
var
  Lst, T, D: Extended;
begin
  D := AMjd - 51544.5;
  T := D / 36525.0;
  Lst := Range360(280.46061837 + 360.98564736629 * D + 0.000387933 * T * T -
    T * T * T / 38710000);
  Result := Lst / 15.0 + Glong / 15;
end;

procedure MiniSun(T: Extended; var ADec, Ra: Extended);
const
  P2 = 6.283185307;
  CosEps = 0.91748;
  SinEps = 0.39778;
var
  L, M, DL, SL, X, Y, Z, RHO: Extended;
begin
  M := P2 * Frac(0.993133 + 99.997361 * T);
  DL := 6893.0 * Sin(M) + 72.0 * Sin(2 * M);
  L := P2 * Frac(0.7859453 + M / P2 + (6191.2 * T + DL) / 1296000);
  SL := Sin(L);
  X := Cos(L);
  Y := CosEps * SL;
  Z := SinEps * SL;
  RHO := Sqrt(1 - Z * Z);
  ADec := (360.0 / P2) * ArcTan2(Z, RHO);
  Ra := (48.0 / P2) * ArcTan2(Y, (X + RHO));
  if Ra < 0 then
    Ra := Ra + 24;
end;

function SinAlt(Mjd0, hour, Glong, Cglat, Sglat: Extended): Extended;
var
  AMjd, T, Ra, ADec, Tau, Salt: Extended;
begin
  AMjd := Mjd0 + hour / 24.0;
  T := (AMjd - 51544.5) / 36525.0;
  MiniSun(T, ADec, Ra);
  Tau := 15.0 * (Lmst(AMjd, Glong) - Ra);
  Salt := Sglat * Sin(RADS * ADec) + Cglat * Cos(RADS * ADec) * Cos(RADS * Tau);
  Result := Salt;
end;

function GetZTTime(Mjd, Tz, Glong: Extended): Extended;
var
  sinho, date, ym, yz, utrise, utset: Extended;
  yp, ye, nz, hour, z1, z2, xe: Extended;
begin
  sinho := Sin(RADS * -0.833);
  date := Mjd - Tz / 24;
  hour := 1.0;
  ym := SinAlt(date, hour - 1.0, Glong, 1, 0) - sinho;

  utrise := 0;
  utset := 0;
  while hour < 25 do
  begin
    yz := SinAlt(date, hour, Glong, 1, 0) - sinho;
    yp := SinAlt(date, hour + 1.0, Glong, 1, 0) - sinho;
    Quad(ym, yz, yp, nz, z1, z2, xe, ye);

    if nz = 1 then
    begin
      if ym < 0.0 then
        utrise := hour + z1
      else
        utset := hour + z1;
    end;

    if nz = 2 then
    begin
      if ye < 0.0 then
      begin
        utrise := hour + z2;
        utset := hour + z1;
      end
      else
      begin
        utrise := hour + z1;
        utset := hour + z2;
      end;
    end;
    ym := yp;
    hour := hour + 2.0;
  end;

  Result := (utrise + utset) / 2;
  if Result < utrise then
    Result := Result + 12;
  if Result > 24 then
    Result := Result - 24;
end;

function DoSunCalc(AMjd: Extended; Glong, Glat: Extended;
  Tz: Integer; out RiseTime, TransitTime, SetTime: TDateTime):
  TCnSunRiseSetType;
var
  sinho, sglat, cglat: Extended;
  yz, yp, ym, nz, z1, z2, xe, ye: Extended;
  utrise, utset, zt: Extended;
  date, hour: Extended;
  rise, sett, above: Boolean;
begin
  sinho := Sin(RADS * -0.833);
  sglat := Sin(RADS * Glat);
  cglat := Cos(RADS * Glat);
  Date := AMjd - Tz / 24;

  rise := False;
  sett := False;
  above := False;
  hour := 1.0;
  utrise := 0;
  utset := 0;
  ym := SinAlt(date, hour - 1.0, Glong, cglat, sglat) - sinho;
  if ym > 0.0 then
    above := True;

  while (hour < 25) and (not sett or not rise) do
  begin
    yz := SinAlt(date, hour, Glong, cglat, sglat) - sinho;
    yp := SinAlt(date, hour + 1.0, Glong, cglat, sglat) - sinho;
    Quad(ym, yz, yp, nz, z1, z2, xe, ye);
    if nz = 1 then
    begin
      if ym < 0.0 then
      begin
        utrise := hour + z1;
        rise := True;
      end
      else
      begin
        utset := hour + z1;
        sett := True;
      end;
    end;

    if nz = 2 then
    begin
      if ye < 0.0 then
      begin
        utrise := hour + z2;
        utset := hour + z1;
      end
      else
      begin
        utrise := hour + z1;
        utset := hour + z2;
      end;
    end;

    ym := yp;
    hour := hour + 2.0;
  end;

  RiseTime := -1;
  TransitTime := -1;
  SetTime := -1;
  if rise or sett then
  begin
    if rise then
      RiseTime := HoursMin(utrise);

    zt := GetZTTime(AMjd, Tz, Glong);
    TransitTime := HoursMin(zt);

    if sett then
      SetTime := HoursMin(utset);

    Result := stNormal;
  end
  else if above then
  begin
    zt := GetZTTime(AMjd, Tz, Glong);
    TransitTime := HoursMin(zt);

    Result := stAllwaysUp;
  end
  else
  begin
    Result := stAllwaysDown;
  end;
end;

// �����ճ�����ʱ��
function GetSunRiseSetTime(ADate: TDateTime; Longitude, Latitude: Extended;
  ZoneTime: Integer; out RiseTime, TransitTime, SetTime: TDateTime):
  TCnSunRiseSetType;
var
  Year, Month, Day: Word;
  Mg: Extended;
begin
  try
    DecodeDate(ADate, Year, Month, Day);
    Mg := Mjd(Year, Month, Day, 0);
    Result := DoSunCalc(Mg, Longitude, Latitude, ZoneTime, RiseTime, TransitTime, SetTime);
  except
    Result := stError;
  end;
end;

// �����ֻ��������, 0-4 ��Ӧ ��ľˮ����
function Get5XingFromNumber(const AValue: Integer): string;
begin
  Result := '';
  if AValue in [0..4] then
    Result := SCn5XingArray[AValue];
end;

// �����ֻ��ʮ������, 0-11
function Get12JianFromNumber(const AValue: Integer): string;
begin
  Result := '';
  if AValue in [0..11] then
    Result := SCn12JianArray[AValue];
end;

// �����ֻ��������, 0-2
function Get3FuFromNumber(const AValue: Integer): string;
begin
  Result := '';
  if AValue in [0..2] then
    Result := SCn3FuArray[AValue];
end;

// �����ֻ��������, 0-1
function GetYinYangFromNumber(const AValue: Integer): string;
begin
  Result := '';
  if AValue in [0..1] then
    Result := SCnYinYangArray[AValue];
end;

// �����ֻ�������,0-9
function GetTianGanFromNumber(const AValue: Integer): string;
begin
  Result := '';
  if (AValue >= 0)  and (AValue < 10) then
    Result := SCnTianGanArray[AValue];
end;

// �����ֻ�õ�֧��, 0-11
function GetDiZhiFromNumber(const AValue: Integer): string;
begin
  Result := '';
  if (AValue >= 0)  and (AValue < 12) then
    Result := SCnDiZhiArray[AValue];
end;

// �����ֻ����ɵ�֧��, 0-59
function GetGanZhiFromNumber(const AValue: Integer): string;
begin
  Result := '';
  if (AValue >= 0)  and (AValue < 60) then
    Result := SCnGanZhiArray[AValue];
end;

// �ӵ�֧���ֻ��ʮ��̫����, 0-11
function Get12TaiSuiFromNumber(const AValue: Integer): string;
begin
  Result := '';
  if (AValue >= 0)  and (AValue < 12) then
    Result := SCn12TaiSuiArray[AValue];
end;

// �����ֻ����ʮ̫����, 0-59
function Get60TaiSuiFromNumber(const AValue: Integer): string;
begin
  Result := '';
  if (AValue >= 0)  and (AValue < 60) then
    Result := SCn60TaiSuiArray[AValue];
end;

// �����ֻ����Ф��, 0-11
function GetShengXiaoFromNumber(const AValue: Integer): string;
begin
  Result := '';
  if (AValue >= 0)  and (AValue < 12) then
    Result := SCnShengXiaoArray[AValue];
end;

// �����ֻ�ý�����, 0-23
function GetJieQiFromNumber(const AValue: Integer): string;
begin
  Result := '';
  if (AValue >= 0)  and (AValue < 24) then
    Result := SCnJieQiArray[AValue];
end;

// �����ֻ��������, 0-11
function GetXingZuoFromNumber(const AValue: Integer): string;
begin
  Result := '';
  if (AValue >= 0)  and (AValue < 12) then
    Result := SCnXingZuoArray[AValue];
end;

// �����ֻ�ö�ʮ������, 0-27
function Get28XiuFromNumber(const AValue: Integer): string;
begin
  Result := '';
  if (AValue >= 0)  and (AValue < 28) then
    Result := SCn28XiuArray[AValue];
end;

// �����ֻ�ö�ʮ����������, 0-27
function Get28XiuLongFromNumber(const AValue: Integer): string;
begin
  Result := '';
  if (AValue >= 0)  and (AValue < 28) then
    Result := SCn28XiuLongArray[AValue];
end;

// �����ֻ��ũ��������, 1-12
function GetLunarMonthFromNumber(const AMonth: Integer; IsLeap: Boolean): string;
begin
  Result := '';
  if (AMonth >= 1) and (AMonth <= 12) then
  begin
    Result := SCnLunarMonthNameArray[AMonth - 1] + SCnLunarMonthName;
    if IsLeap then
      Result := SCnLunarMonthLeapName + Result;
  end;
end;

// �����ֻ��ũ��������, 1-30
function GetLunarDayFromNumber(const ADay: Integer): string;
var
  D1, D2: Integer;
begin
  Result := '';
  if ADay in [1..30] then
  begin
    D2 := ADay div 10; // ʮλ
    D1 := ADay mod 10; // ��λ
    // ��������
    if D1 = 0 then
    begin
      case D2 of
      1:
        begin
          D2 := 0;
          D1 := 10; // ��ʮ
        end;
      2, 3:
        begin
          Inc(D2, 2);
          D1 := 10; // һ�㲻������إ��ئ�����Ƕ�ʮ��ʮ��
        end;
      end;
    end;
    Result := SCnLunarNumber2Array[D2] + SCnLunarNumber1Array[D1 - 1];
  end;
end;

// ����ɻ��������, 0-9 ת���� 0-1
function GetYinYangFromGan(const Gan: Integer): Integer;
begin
  Result := -1;
  if Gan in [0..9] then // �������������������Դ�����
    Result := 1 - (Gan mod 2);
end;

// �ӵ�֧���������, 0-11 ת���� 0-1
function GetYinYangFromZhi(const Zhi: Integer): Integer;
begin
  Result := -1;
  if Zhi in [0..11] then // ������������î�����Դ�����
    Result := 1 - (Zhi mod 2);
end;

// ����ɵ�֧��ϳɸ�֧��0-9 0-11 ת���� 0-59��ע������ʮ���ţ���������������֧�������
function CombineGanZhi(Gan, Zhi: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  if (Gan in [0..9]) and (Zhi in [0..11]) then
  begin
    for I := 0 to 6 do
    begin
      if (I * 10 + Gan) mod 12 = Zhi then
      begin
        Result := I * 10 + Gan;
        Exit;
      end;
    end;
  end;
end;

// ����֧��ֳ���ɵ�֧��0-59 ת���� 0-9 0-11
function ExtractGanZhi(GanZhi: Integer; out Gan: Integer; out Zhi: Integer): Boolean;
begin
  if GanZhi in [0..59] then
  begin
    Result := True;
    Gan := GanZhi mod 10;
    Zhi := GanZhi mod 12;
  end
  else
  begin
    Result := False;
    Gan := -1;
    Zhi := -1;
  end;
end;

// ���ĳ�ɵ����У�0-4 ��Ӧ ��ľˮ����
function Get5XingFromGan(const Gan: Integer): Integer;
begin
  case Gan div 2 of
    0: Result := 1; // ����ľ
    1: Result := 3; // ������
    2: Result := 4; // �켺��
    3: Result := 0; // ������
    4: Result := 2; // �ɹ�ˮ
  else
    Result := -1;
  end;
end;

// ���ĳ֧�����У�0-4 ��Ӧ ��ľˮ����
function Get5XingFromZhi(const Zhi: Integer): Integer;
begin
  case Zhi of
    8, 9: Result := 0;
    2, 3: Result := 1;
    0,11: Result := 2;
    5, 6: Result := 3;
    1, 4, 7, 10: Result := 4;
  else
    Result := -1;
  end;
end;

// ����ɻ��̫������ֵ���ڲ�ʹ��
function GetTaiXuanPeiShuFromGan(const Gan: Integer): Integer;
begin
  Result := -1;
  if Gan in [0..9] then
    Result := SCnTaiXuanPeiShuArray[Gan mod 5];
end;

// �ӵ�֧���̫������ֵ���ڲ�ʹ��
function GetTaiXuanPeiShuFromZhi(const Zhi: Integer): Integer;
begin
  Result := -1;
  if Zhi in [0..11] then
    Result := SCnTaiXuanPeiShuArray[Zhi mod 6];
end;

// ���ĳ��֧���������У��̣���0-4 ��Ӧ ��ľˮ����
function Get5XingFromGanZhi(const GanZhi: Integer): Integer; overload;
var
  Gan, Zhi: Integer;
begin
  ExtractGanZhi(GanZhi, Gan, Zhi);
  Result := Get5XingFromGanZhi(Gan, Zhi);
end;

// ���ĳ��֧���������У��̣���0-4 ��Ӧ ��ľˮ����
function Get5XingFromGanZhi(Gan, Zhi: Integer): Integer; overload;
var
  TaiXuan1, TaiXuan2, TaiXuan3, TaiXuan4: Integer; // �ĸ�̫������
begin
  // �˴���֧����ͬΪ������ͬΪż��������ԷǷ�
  Result := -1;
  if (Gan + Zhi) mod 2 = 0 then
  begin
    TaiXuan1 := GetTaiXuanPeiShuFromGan(Gan);
    TaiXuan2 := GetTaiXuanPeiShuFromZhi(Zhi);
    if Gan mod 2 = 0 then
    begin
      // ż��Ϊ���ԣ�ȡ����
      Inc(Gan);
      Inc(Zhi);
    end
    else // �棬Ϊ���ԣ�ȡ����
    begin
      Dec(Gan);
      Dec(Zhi);
    end;
    TaiXuan3 := GetTaiXuanPeiShuFromGan(Gan);
    TaiXuan4 := GetTaiXuanPeiShuFromZhi(Zhi);

    Result := (TaiXuan1 + TaiXuan2 + TaiXuan3 + TaiXuan4) mod 5;
    case Result of // ����ӳ�䣬һΪ�𣬶�Ϊ������Ϊľ����Ϊ���壨0��Ϊˮ��
      0: Result := 2;
      1: Result := 3;
      2: Result := 4;
      3: Result := 1;
      4: Result := 0;
    end;
  end;
end;

// ���ĳ�����յ��������У��̣���0-4 ��Ӧ ��ľˮ����
function Get5XingFromDay(AYear, AMonth, ADay: Integer): Integer;
var
  Gan, Zhi: Integer;
begin
  ExtractGanZhi(GetGanZhiFromDay(AYear, AMonth, ADay), Gan, Zhi);
  Result := Get5XingFromGanZhi(Gan, Zhi);
end;

// ���ĳ��֧���������У������������ַ���
function Get5XingLongFromGanZhi(const GanZhi: Integer): string; overload;
var
  I: Integer;
begin
  I := GanZhi div 2;
  if I in [0..29] then
    Result := SCnNaYinWuXingArray[I]
  else
    Result := '';
end;

// ���ĳ��֧���������У������������ַ���
function Get5XingLongFromGanZhi(Gan, Zhi: Integer): string; overload;
var
  GanZhi: Integer;
begin
  GanZhi := CombineGanZhi(Gan, Zhi);
  Result := Get5XingLongFromGanZhi(GanZhi);
end;
// ���ĳ�����յ��������У������������ַ���
function Get5XingLongFromDay(AYear, AMonth, ADay: Integer): string;
var
  Gan, Zhi: Integer;
begin
  ExtractGanZhi(GetGanZhiFromDay(AYear, AMonth, ADay), Gan, Zhi);
  Result := Get5XingLongFromGanZhi(Gan, Zhi);
end;

// ���ĳ��֧�����������ϵ�֧
function Get3HeFromZhi(const Zhi: Integer; out He1: Integer;
  out He2: Integer): Boolean;
begin
  // �����ǻ���� 4 ��������֧��Ҳ�������ӳ��������硢���ϳ�î��δ��
  // ����˵���������������߼�ţ��������
  Result := False;
  if Zhi in [0..11] then
  begin
    case Zhi of
      0:  begin He1 := 8;  He2 := 4;  end;
      1:  begin He1 := 5;  He2 := 9;  end;
      2:  begin He1 := 6;  He2 := 10; end;
      3:  begin He1 := 11; He2 := 7;  end;
      4:  begin He1 := 8;  He2 := 0;  end;
      5:  begin He1 := 9;  He2 := 1;  end;
      6:  begin He1 := 2;  He2 := 10; end;
      7:  begin He1 := 3;  He2 := 11; end;
      8:  begin He1 := 0;  He2 := 4;  end;
      9:  begin He1 := 5;  He2 := 1;  end;
      10: begin He1 := 2;  He2 := 6;  end;
      11: begin He1 := 3;  He2 := 7;  end;
    else
      He1 := -1;
      He2 := -1;
    end;
    Result := True;
  end;
end;

// ���ݹ��������жϵ�ʱ����
function GetCalendarType(AYear, AMonth, ADay: Integer): TCnCalendarType;
begin
  if AYear > 1582 then
    Result := ctGregorian
  else if AYear < 1582 then
    Result := ctJulian
  else if AMonth < 10 then
    Result := ctJulian
  else if (AMonth = 10) and (ADay <= 4) then
    Result := ctJulian
  else if (AMonth = 10) and (ADay in [5..14]) then
    Result := ctInvalid
  else
    Result := ctGregorian;
end;

// ����ĳ�����Ƿ�����
function GetIsLeapYear(AYear: Integer): Boolean;
begin
  if GetCalendarType(AYear, 1, 1) = ctGregorian then
    Result := (AYear mod 4 = 0) and ((AYear mod 100 <> 0) or (AYear mod 400 = 0))
  else if AYear > 0 then
    Result := (AYear mod 4 = 0)
  else if AYear < 0 then // ��Ҫ�����жϹ�Ԫǰ��ԭ����û�й�Ԫ 0 ��
    Result := (AYear - 3) mod 4 = 0
  else
    raise ECnDateTimeException.Create(SCnErrorYearIsInvalid);
end;

// ȡ���������������� 1582 �� 10 �µ��������
function GetMonthDays(AYear, AMonth: Integer): Integer;
begin
  case AMonth of
    1,3,5,7,8,10,12:
      Result := 31;
    4,6,9,11:
      Result:= 30;
    2:// ����
      if (AYear <> 0) and GetIsLeapYear(AYear) then
        Result := 29
      else
        Result := 28 // û�й�Ԫ 0 �꣬����ƽ�괦��
  else
    Result := 0;
  end;
end;

// ���ĳũ�������ʷ�����ӵ����£����� 1~12 ��Ӧһ�µ�ʮ���£����� 0 ��ʾ�޶�������
function GetLunarAdditionalLeapMonth(AYear: Integer): Integer;
begin
  if (AYear in [23, 239]) then // ����ũ�����������ʮ����
    Result := 12
  else
    Result := 0;
end;

// ȡũ�����ĳ������
function GetLunarMonthDays(ALunarYear, ALunarMonth: Integer;
  IsLeapMonth: Boolean = False): Integer;
var
  EquDay1, EquDay2: Integer;
  AYear, AMonth, ADay: Integer;
  ALeap: Boolean;
begin
  Result := -1;
  if IsLeapMonth and (GetLunarLeapMonth(ALunarYear) <> ALunarMonth)
    and (GetLunarAdditionalLeapMonth(ALunarYear) <> ALunarMonth) then
    Exit; // �����޴����»�����������˳�

  if not GetDayFromLunar(ALunarYear, ALunarMonth, 1, IsLeapMonth, AYear, AMonth, ADay) then
    Exit;

  EquDay1 := GetEquStandardDays(AYear, AMonth, ADay);

  ALeap := False;
  if (GetLunarLeapMonth(ALunarYear) = ALunarMonth) or
    (GetLunarAdditionalLeapMonth(ALunarYear) = ALunarMonth) then // ������ڱ��������и����µ�
  begin
    if IsLeapMonth then // �������������£������һ����
    begin
      Inc(ALunarMonth);
      if ALunarMonth > 12 then
      begin
        Dec(ALunarMonth, 12);
        Inc(ALunarYear);
      end;
    end
    else
      ALeap := True; // ��һ����������
  end
  else
  begin
    Inc(ALunarMonth);
    if ALunarMonth > 12 then
    begin
      Dec(ALunarMonth, 12);
      Inc(ALunarYear);
    end;
  end;

  if not GetDayFromLunar(ALunarYear, ALunarMonth, 1, ALeap, AYear, AMonth, ADay) then
    Exit;

  EquDay2 := GetEquStandardDays(AYear, AMonth, ADay);
  Result := EquDay2 - EquDay1;
end;

// ���ع��������Ƿ�Ϸ�
function GetDateIsValid(AYear, AMonth, ADay: Integer): Boolean;
begin
  Result := (AYear <> 0) and (AMonth in [1..12]) and (ADay > 0)
    and (ADay <= GetMonthDays(AYear, AMonth));
  if Result and (AYear = 1582) and (AMonth = 10) then
    Result := not (ADay in [5..14]);
end;

// �жϹ��������Ƿ�Ϸ������Ϸ����׳��쳣
procedure ValidDate(AYear, AMonth, ADay: Integer);
begin
  if not GetDateIsValid(AYear, AMonth, ADay) then
    raise ECnDateTimeException.CreateFmt(SCnErrorDateIsInvalid, [AYear, AMonth, ADay]);
end;

// ����ũ�������Ƿ�Ϸ�
function GetLunarDateIsValid(ALunarYear, ALunarMonth, ALunarDay: Integer;
  IsLeapMonth: Boolean): Boolean;
begin
  Result := False;
  if (ALunarYear = 0) or not (ALunarMonth in [1..12]) then
    Exit;

  if ALunarDay > 30 then
    Exit;

  if IsLeapMonth and (GetLunarLeapMonth(ALunarYear) <> ALunarMonth)
    and (GetLunarAdditionalLeapMonth(ALunarYear) <> ALunarMonth) then
    Exit; // �����޴����»�����������˳�

  // �жϴ�С�����Ƿ񳬽�
  if ALunarDay = 30 then
  begin
    if ALunarDay > GetLunarMonthDays(ALunarYear, ALunarMonth, IsLeapMonth) then
      Exit;
  end;

  Result := True;
end;

// �ж�ũ�������Ƿ�Ϸ������Ϸ����׳��쳣
procedure ValidLunarDate(ALunarYear, ALunarMonth, ALunarDay: Integer; IsLeapMonth: Boolean);
begin
  if not GetLunarDateIsValid(ALunarYear, ALunarMonth, ALunarDay, IsLeapMonth) then
    raise ECnDateTimeException.CreateFmt(SCnErrorLunarDateIsInvalid,
      [ALunarYear, ALunarMonth, ALunarDay, Integer(IsLeapMonth)]);
end;

// ����ʱ���Ƿ�Ϸ�
function GetTimeIsValid(AHour, AMinitue, ASecond: Integer): Boolean;
begin
  Result := (AHour in [0..23]) and (AMinitue in [0..59]) and (ASecond in [0..59]);
end;

// �ж�ʱ���Ƿ�Ϸ������Ϸ����׳��쳣
procedure ValidTime(AHour, AMinitue, ASecond: Integer);
begin
  if not GetTimeIsValid(AHour, AMinitue, ASecond) then
    raise ECnDateTimeException.CreateFmt(SCnErrorTimeIsInvalid, [AHour, AMinitue, ASecond]);
end;

// �������������󲽽�һ�죬���Ǹ������ꡢ���������ɾ 10 �������
procedure StepToNextDay(var AYear, AMonth, ADay: Integer; ZeroYear: Boolean);
var
  LY: Integer;
begin
  if not ZeroYear then
    ValidDate(AYear, AMonth, ADay);

  if (AYear = 1582) and (AMonth = 10) and (ADay = 4) then
  begin
    ADay := 15;
    Exit;
  end
  else
  begin
    if ZeroYear and (AYear <= 0) then   // GetIsLeapYear �� GetMonthDays ֻ���ܷ� 0 ����
      LY := AYear - 1
    else
      LY := AYear;

    if (AMonth = 2) and (ADay = 28) then // ��������� 2 �� 29 ��
    begin
      if GetIsLeapYear(LY) then
        ADay := 29
      else
      begin
        ADay := 1;
        Inc(AMonth);
      end;
      Exit;
    end
    else
    begin
      if ADay >= GetMonthDays(LY, AMonth) then // �µף�����
      begin
        ADay := 1;
        Inc(AMonth);

        if AMonth = 13 then  // ���£�����
        begin
          AMonth := 1;

          if not ZeroYear and (AYear = -1) then // ��Ԫǰһ�굽��ԪԪ��
            AYear := 1
          else
            Inc(AYear);
        end;
      end
      else
      begin
        Inc(ADay); // ���µף���һ�����
      end;
    end;
  end;
end;

// �Ƚ������������ڣ�1 >=< 2 �ֱ𷵻� 1��0��-1
function Compare2Day(Year1, Month1, Day1, Year2, Month2, Day2: Integer): Integer;
begin
  ValidDate(Year1, Month1, Day1);
  ValidDate(Year2, Month2, Day2);

  if Year1 > Year2 then // ���
  begin
    Result := 1
  end
  else if Year1 = Year2 then // ���
  begin
    if Month1 > Month2 then  // ����´�
    begin
      Result := 1
    end
    else if Month1 = Month2 then // ����µ�
    begin
      if Day1 > Day2 then // ����µ��մ�
      begin
        Result := 1
      end
      else if Day1 = Day2 then // ����µ��յ�
      begin
        Result := 0;
      end
      else  // ����µ���С
      begin
        Result := -1;
      end;
    end
    else // �����С
    begin
      Result := -1;
    end;
  end
  else // ��С
  begin
    Result := -1;
  end;
end;

// �Ƚ�����ũ�����ڣ�����������Ϣ����1 >=< 2 �ֱ𷵻� 1��0��-1
function Compare2LunarDay(Year1, Month1, Day1: Integer; IsLeap1: Boolean;
  Year2, Month2, Day2: Integer; IsLeap2: Boolean): Integer;
begin
  if Year1 > Year2 then // ���
  begin
    Result := 1
  end
  else if Year1 = Year2 then // ���
  begin
    if Month1 > Month2 then  // ����´�
    begin
      Result := 1
    end
    else if Month1 = Month2 then // ����µ�
    begin
      if IsLeap1 = IsLeap2 then // ��Ҳ��
      begin
        if Day1 > Day2 then // ����µ��մ�
        begin
          Result := 1
        end
        else if Day1 = Day2 then // ����µ��յ�
        begin
          Result := 0;
        end
        else  // ����µ���С
        begin
          Result := -1;
        end;
      end
      else if IsLeap1 and not IsLeap2 then // ��ĳ�´���ĳ��
        Result := 1
      else
        Result := -1;
    end
    else // �����С
    begin
      Result := -1;
    end;
  end
  else // ��С
  begin
    Result := -1;
  end;
end;

// ȡĳ���ڵ������������������ 1582 �� 10 �µ��������
function GetDayFromYearBegin(AYear, AMonth, ADay: Integer): Integer;
const
  MonthAbsDays: array [Boolean] of TDayTable =
    ((0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334),
     (0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335));
begin
  Result := MonthAbsDays[GetIsLeapYear(AYear)][AMonth] + ADay;
end;

// ȡĳ���ڵ������������Сʱ���֡�����������С���������� 1582 �� 10 �µ��������
function GetDayFromYearBegin(AYear, AMonth, ADay, AHour: Integer;
  AMinute, ASecond: Integer): Extended;
begin
  Result := GetDayFromYearBegin(AYear, AMonth, ADay);
  Result := Result + (AHour / 24.0) + (AMinute / 1440.0) + (ASecond / 86400.0);
end;

// �Ӿ��������������º���������������ж��Ƿ������꣬���� False ��ʾ���Ϸ�����
function ExtractMonthDay(Days: Integer; AYear: Integer; out AMonth: Integer;
  out ADay: Integer): Boolean;
var
  I, Day: Integer;
begin
  if (Days <= 0) or (Days > 366) or ((Days > 365) and (not GetIsLeapYear(AYear))) then
  begin
    Result := False;
    AMonth := -1;
    ADay := -1;
    Exit;
  end;

  for I := 1 to 12 do
  begin
    Day := GetMonthDays(AYear, I);
    if Days > Day then
      Days := Days - Day
    else
    begin
      AMonth := I;
      Break;
    end;
  end;
  ADay := Floor(Days);
  Result := True;
end;

function GetBasicDays(AYear, AMonth, ADay: Integer): Integer;
var
  I: Integer;
begin
  if AYear > 0 then
    Result := (AYear - 1) * 365
  else
    Result := AYear * 365;

  for I := 1 to AMonth - 1 do
    Inc(Result, GetMonthDays(AYear, I));
  Inc(Result, ADay);
end;

function GetLeapDays(AYear, AMonth, ADay: Integer): Integer;
begin
  if AYear >= 0 then // ��Ԫ��
  begin
    if GetCalendarType(AYear, AMonth, ADay) in [ctJulian, ctInvalid] then
      Result := 0
    else
    begin
      // 1582.10.5/15 ǰ��������ֻ������һ���������պ����Ϊ���������
      Result := 10; // ���������ɾȥ�� 10 ��

      if AYear > 1700 then // �����㷨�򻯰棬�� 1701 ��� 11 ��
      begin
        // ÿһ�����ۼ�һ
        Inc(Result, 1 + ((AYear - 1701) div 100));
        // �� 400 ���������Ͳ���
        Dec(Result, ((AYear - 1601) div 400));
      end;
    end;
    Result := ((AYear - 1) div 4) - Result; // 4 ��һ����
  end
  else // ��Ԫǰ
  begin
    Result := - ((- AYear + 3) div 4);
  end;
end;

// ��þ๫Ԫԭ�������
function GetAllDays(AYear, AMonth, ADay: Integer): Integer;
begin
  Result := GetBasicDays(AYear, AMonth, ADay) + GetLeapDays(AYear, AMonth, ADay);
end;

// ��ȡ��Ч��׼�������˸���ϵ��ֲ�������ƺ��Ǿ���������Ԫ��Ԫ��������
// ע��˴��ĸ��������������ɾȥ�� 10 �죬��˵�Ч��׼����������
function GetEquStandardDays(AYear, AMonth, ADay: Integer): Integer;
var
  AType: TCnCalendarType;
begin
  Result := 0;
  AType := GetCalendarType(AYear, AMonth, ADay);
  if AType = ctGregorian then
  begin
    Result := (AYear - 1) * 365 + ((AYear - 1) div 4) -((AYear - 1) div 100)
     + ((AYear - 1) div 400) + GetDayFromYearBegin(AYear, AMonth, ADay);
  end
  else if AType = ctJulian then
  begin
    { Ϊɶ���� 2���²⹫Ԫ 1 �굽 1582 �꣬�������ϸ�������������� 12 �죬
      (100, 200, 300, 500, 600, 700, 900, 1000, 1100, 1300, 1400, 1500)
      ���������ֻɾȥ 10 �죬���������� 2 ��Ĳ�ֵ��
      ��˵����������������� 1582.10.4 ��ǰ���Ƶõĸ��������Ԫ��Ԫ��
      ��ʵ�ʹ�ԪԪ��Ԫ������ͬһ�졣 }
    if AYear > 0 then
      Result := (AYear - 1) * 365 + ((AYear - 1) div 4)
        + GetDayFromYearBegin(AYear, AMonth, ADay) - 2
    else
      Result := (AYear - 1) * 365 + ((AYear) div 4) - 1     // �����һ����Ϊ 0 ��Ҳ�����꣬��һ��
        + GetDayFromYearBegin(AYear - 1, AMonth, ADay) - 2;
    // GetDayFromYearBegin ��Ҫ�����ķ� 0 �����꣬�� AYear ����������������Ҫ��һ
  end;
end;

// ��õ�Ч��׼������Ӧ��ĳ�����գ����ƶ���
function GetDayFromEquStandardDays(EquDays: Integer;
  out AYear, AMonth, ADay: Integer): Boolean;
const
  D1 = 365;                 // ������            365
  D4 = D1 * 4 + 1;          // �����һ����      1461
  D100 = D4 * 25 - 1;       // ���겻���һ��    36524
  D400 = D100 * 4 + 1;      // �İ��������һ��  146101
  MORE_LEAP_DAYS: array[1..12] of Integer = // ����������� 12 ��� 2 �� 29 ��Ӧ�ĵ�Ч��׼��
    (36217, 72742, 109267, 182317, 218842, 255367, 328417, 364942, 401467, 474517, 511042, 547567);
  // Ҳ����Ԫ 100, 200, 300, 500, 600, 700, 900, 1000, 1100, 1300, 1400, 1500 �� 2 �� 29 �������������������
var
  Diff: Integer;
  Y, M, D, I: Word;
  T: Integer;
  DayTable: PDayTable;
  IsJunlian229: Boolean;

  // �ж� 0 ����������Ƿ��������������������꣬Ҳ���Ƿ���ĵİ���
  function IsJulianNotGregorianLeap(AYear: Integer): Boolean;
  begin
    Result := ((AYear mod 100) = 0) and ((AYear mod 400) <> 0);
  end;

begin
  Result := False;
  AYear := 0;
  AMonth := 0;
  ADay := 0;

  if EquDays < 0 then Exit;  // �ݲ�����Ԫǰ�ĵ�Ч��׼��

  IsJunlian229 := False;
  if EquDays <= 577735 then  // ����� 1582.10.4 (577735) ��֮ǰΪ����������Ҫ��¼�Ƿ����� 2 �� 29 ���Լ�����
  begin
    // ����� 36217 Ҳ�� ��Ԫ 100 �� 2 �� 29 �յľ���
    Diff := EquDays - 36217;
    if Diff < 0 then
      Diff := -Diff;

    if (Diff mod 36525) = 0 then
    begin
      Diff := Diff div 36525; // ���걶��
      Inc(Diff);
      if Diff mod 4 <> 0 then
        IsJunlian229 := True;   // �������������������� 2 �� 29��
    end;

    Diff := 0;
    for I := High(MORE_LEAP_DAYS) downto Low(MORE_LEAP_DAYS) do
    begin
      if EquDays > MORE_LEAP_DAYS[I] then
      begin
        Diff := I;
        Break;
      end;
    end;

    // Diff := EquDays div (365 * 100) - EquDays div (365 * 400);
    Dec(EquDays, 10);        // �������ɾȥ�� 10 ��
    Inc(EquDays, 12 - Diff); // ���϶���� 12 ���ж���Ĳ���
  end;

  T := EquDays;
  Y := 1;
  while T >= D400 do         // ���˼����İ�������
  begin
    Dec(T, D400);
    Inc(Y, 400);             // �۵��İ���������
  end;

  I := T div D100;           // ���˼������겻��
  D := T mod D100;
  if I = 4 then              // ��������ĸ����겻��
  begin
    Dec(I);
    Inc(D, D100);
  end;
  Inc(Y, I * 100);

  I := D div D4;             // �ֹ��˼�������һ��
  D := D mod D4;
  Inc(Y, I * 4);

  I := D div D1;             // �ֹ��˼�������
  D := D mod D1;
  if I = 4 then
  begin
    Dec(I);
    Inc(D, D1);
  end;
  Inc(Y, I);

  DayTable := @MonthDays[(Y mod 4 = 0) and ((Y mod 100 <> 0) or (Y mod 400 = 0))];
  // ע�ⲻ���� GetIsLeapYear(Y) ������Ժ͸�������Ļ���ж�
  // ���ݶ�����ô��ĸ���������������ж�

  M := 1;
  if D > 0 then
  begin
    while True do
    begin
      I := DayTable^[M];

      if IsJunlian229 and (M = 2) then // �����ض�������������
        I := 29;

      if D <= I then
        Break;
      Dec(D, I);
      Inc(M);
    end;
  end
  else
  begin
    // ��� D ǡ�������� 0��˵�� 1 �� 0 �վ�����һ��� 12 �� 31 ��
    Dec(Y);
    M := 12;
    D := 31;
  end;

  AYear := Y;
  AMonth := M;
  ADay := D;

  Result := True;
end;

function GetJulianDate(AYear, AMonth, ADay: Integer): Extended;
var
  A, Y, M: Integer;
begin
  ValidDate(AYear, AMonth, ADay);

  // ��繫Ԫû�й�Ԫ 0 �꣬���˴��ڲ���ݼ���Ҫ�����������ڲ��ѹ�Ԫǰ���������һ
  NonZeroYearToZeroYear(AYear);

  A := (14 - AMonth) div 12;
  Y := AYear + 4800 - A;
  M := AMonth + 12 * A - 3;

  if GetCalendarType(AYear, AMonth, ADay) = ctGregorian then
    Result := ADay + ((153 * M + 2) div 5) + 365 * Y + (Y div 4) - (Y div 100) + (Y div 400) - 32045
  else
    Result := ADay + ((153 * M + 2) div 5) + 365 * Y + (Y div 4) - 32083;
end;

function GetJulianDate(AYear, AMonth, ADay: Integer;
  AHour, AMinute, ASecond: Integer): Extended; overload;
begin
  Result := GetJulianDate(AYear, AMonth, ADay) - 0.5; // �õ� 0 ʱ����������
  Result := Result + (AHour * 3600 + AMinute * 60 + ASecond) / 86400; // ���ϵ���С��
end;

function GetModifiedJulianDate(AYear, AMonth, ADay: Integer): Extended;
begin
  Result := GetJulianDate(AYear, AMonth, ADay) - 2400000.5;
end;

function GetModifiedJulianDate(AYear, AMonth, ADay: Integer;
  AHour, AMinute, ASecond: Integer): Extended;
begin
  Result := GetJulianDate(AYear, AMonth, ADay, AHour, AMinute, ASecond) - 2400000.5;
end;

function GetDayFromJulianDate(JD: Extended; out AYear, AMonth, ADay: Integer): Boolean;
var
  A, B, C, D, E, M: Double;
begin
  // Jean Meeus ת���㷨
  A := JD;
  if A < 2299161 then          // �ж��Ƿ��ڸ������������ǰ
    B := A
  else
  begin
    C := Trunc((A - 1867216.25)/36524.25); // ����������������
    B := A + 1 + C - Trunc(C / 4);         // ��������������
  end;

  D := B + 1524;                  // ������Ԫ����Ԫǰ 4716 �� 3 �� 1 ��
  C := Trunc((D - 122.1)/365.25); // ������ݻ���
  E := Trunc(365.25 * C);         // �����
  M := Trunc((D - E)/30.6001);    // �����·ݻ���

  // �����������
  ADay := Trunc(D - E - Trunc(30.6001 * M)); // ��������
  AMonth := Trunc(M - 1);                    // �����·�ƫ��
  if AMonth > 12 then
    AMonth := AMonth - 12;        // ���� 12 �º���·�

  AYear := Trunc(C - 4715);       // ��ݻ���ת��
  if AMonth > 2 then              // ���� 1-2 ������ǰһ������
    Dec(AYear);

  // ����õ�������������ݣ���ֵת��Ϊ�޹�Ԫ 0 ������
  ZeroYearToNonZeroYear(AYear);

  Result := GetDateIsValid(AYear, AMonth, ADay);
end;

function GetDayFromModifiedJulianDate(MJD: Extended; out AYear, AMonth, ADay: Integer): Boolean;
var
  JD: Extended;
begin
  // Լ��������ת��׼������
  JD := MJD + 2400000.5;
  Result := GetDayFromJulianDate(JD, AYear, AMonth, ADay);
end;

// ���ĳ���������ڼ���0-6
function GetWeek(const AValue: TDateTime): Integer; overload;
var
  Year, Month, Day: Word;
begin
  DecodeDate(AValue, Year, Month, Day);
  // -2 Դ�ڹ�Ԫ 1 �� 1 �� 2 �ղ���������
  Result := (GetAllDays(Year, Month, Day) - 2) mod 7;
  if Result < 0 then
    Inc(Result, 7);
end;

// ���ĳ���������ڼ���0-6
function GetWeek(AYear, AMonth, ADay: Integer): Integer; overload;
begin
  // -2 Դ�ڹ�Ԫ 1 �� 1 �� 2 �ղ���������
  Result := (GetAllDays(AYear, AMonth, ADay) - 2) mod 7;
  if Result < 0 then
    Inc(Result, 7);
end;

// �����ֻ�������������������ڶ���, 0-6 ��Ӧ �յ���
function GetWeekFromNumber(const AValue: Integer): string;
begin
  Result := '';
  if AValue in [0..6] then
    Result := SCnWeekNumberArray[AValue];
end;

// ============================= ���������㷨 ==================================

// �����㷨֮���ĳ�������ڵĵ� N �������������������1-24����ӦС������������������Ϊ 0
// ������ 1582 ��֮ǰ������ʮ��ƫ�������������겻��Ϊ 0
function GetJieQiDayTimeFromYear(AYear, N: Integer): Extended;
var
  JuD, Tht, YrD, ShuoD: Extended;
begin
  { ���ڽ����¶�����ɵ�����Ӱ�죬̫������ͨ�������������ʱ�䲢����һ��ȷ�ع���
    ����û���� 365.2422 Ϊ������ֱ�Ӽ������������ʱ�̡����������ʽ����ֲ������
    ���ص�������С���������������ʱ���룬�����Ȳ����ߣ������ 10 �������ң��������塣}

  // ��û�й�Ԫ 0 ��ĵ������Ա�������Ĺ����깩���Ľ��м���
  NonZeroYearToZeroYear(AYear);

  JuD := AYear * (365.2423112 - 6.4e-14 * (AYear-100) * (AYear - 100)
    - 3.047e-8 * (AYear-100)) + 15.218427 * N + 1721050.71301;
  Tht := 3e-4 * AYear - 0.372781384 - 0.2617913325 * N;
  YrD := (1.945 * Sin(Tht) - 0.01206 * Sin(2 * Tht)) * (1.048994 - 2.583e-5 * AYear);
  ShuoD := -18e-4 * Sin(2.313908653 * AYear - 0.439822951 - 3.0443 * N);
  Result := JuD + YrD + ShuoD - GetEquStandardDays(AYear, 1, 0) - 1721425; // ����
  // (juD - GetEquStandardDays(AYear, 1, 0) - 1721425); ƽ��
  if AYear <= 1582 then // 1582 �걻ɾ���� 10 �죬Ҫ�ӻ���
    Result := Result + 10;
end;

// =========================== ������ȷ�㷨��ʼ ================================

const
  CN_PI: Extended = 3.1415926535897932384626;

  RAD = 180 * 3600 / PI;   // ÿ���ȵĽ�����

  J2000 = 2451545;

  CN_DT_AT: array[0..101] of Extended = ( // TD - UT1 ���һ��ļ����
    -4000,108371.7,-13036.80,392.000, 0.0000,
     -500, 17201.0,  -627.82, 16.170,-0.3413,
     -150, 12200.6,  -346.41,  5.403,-0.1593,
      150,  9113.8,  -328.13, -1.647, 0.0377,
      500,  5707.5,  -391.41,  0.915, 0.3145,
      900,  2203.4,  -283.45, 13.034,-0.1778,
     1300,   490.1,   -57.35,  2.085,-0.0072,
     1600,   120.0,    -9.81, -1.532, 0.1403,
     1700,    10.2,    -0.91,  0.510,-0.0370,
     1800,    13.4,    -0.72,  0.202,-0.0193,
     1830,     7.8,    -1.81,  0.416,-0.0247,
     1860,     8.3,    -0.13, -0.406, 0.0292,
     1880,    -5.4,     0.32, -0.183, 0.0173,
     1900,    -2.3,     2.06,  0.169,-0.0135,
     1920,    21.2,     1.69, -0.304, 0.0167,
     1940,    24.2,     1.22, -0.064, 0.0031,
     1960,    33.2,     0.51,  0.231,-0.0109,
     1980,    51.0,     1.29, -0.026, 0.0032,
     2000,    63.87,    0.1,   0,     0,
     2005,    64.7,     0.4,   0,     0,   //һ�����Ϊx,�� 10x=0.4��/��*(2015-2005),���x=0.4
     2015,    69
    );

  nutB: array[0..49] of Extended = ( // �о����¶������
    2.1824,  -33.75705, 36e-6,-1720,920,
    3.5069, 1256.66393, 11e-6,-132, 57,
    1.3375,16799.4182, -51e-6, -23, 10,
    4.3649,  -67.5141,  72e-6,  21, -9,
    0.04,   -628.302,   0,     -14,  0,
    2.36,   8328.691,   0,       7,  0,
    3.46,   1884.966,   0,      -5,  2,
    5.44,  16833.175,   0,      -4,  2,
    3.69,  25128.110,   0,      -3,  0,
    3.55,    628.362,   0,       2,  0
  );

  XL0: array[0..2665] of Extended = (
  {* �˴������������ݱ��еĵ��򲿷ּ����ݱ�ļ���
    Dear ���ȣ�J2000+-4ǧ�� �ƾ�0.1���� ��γ0.1���� ����0.1AU/10^6 }
    10000000000,// A �ı���
    20,578,920,1100,1124,1136,1148,1217,1226,1229,1229,1229,1229,1937,2363,2618,2633,2660,2666,// λ��������
    {L0}
    17534704567,0.00000000000,0.00000000000,334165646,4.669256804,6283.075849991,3489428,4.6261024,12566.1517000,349706,
    2.744118,5753.384885,341757,2.828866,3.523118,313590,3.627670,77713.771468,267622,4.418084,7860.419392,234269,6.135162,
    3930.209696,132429,0.742464,11506.769770,127317,2.037097,529.690965,119917,1.109629,1577.343542,99025,5.23268,5884.92685,
    90186,2.04505,26.29832,85722,3.50849,398.14900,77979,1.17883,5223.69392,75314,2.53339,5507.55324,50526,4.58293,18849.22755,
    49238,4.20507,775.52261,35666,2.91954,0.06731,31709,5.84902,11790.62909,28413,1.89869,796.29801,27104,0.31489,10977.07880,
    24281,0.34481,5486.77784,20616,4.80647,2544.31442,20539,1.86948,5573.14280,20226,2.45768,6069.77675,15552,0.83306,
    213.29910,13221,3.41118,2942.46342,12618,1.08303,20.77540,11513,0.64545,0.98032,10285,0.63600,4694.00295,10190,0.97569,
    15720.83878,10172,4.26680,7.11355,9921,6.2099,2146.1654,9761,0.6810,155.4204,8580,5.9832,161000.6857,8513,1.2987,6275.9623,
    8471,3.6708,71430.6956,7964,1.8079,17260.1547,7876,3.0370,12036.4607,7465,1.7551,5088.6288,7387,3.5032,3154.6871,7355,
    4.6793,801.8209,6963,0.8330,9437.7629,6245,3.9776,8827.3903,6115,1.8184,7084.8968,5696,2.7843,6286.5990,5612,4.3869,
    14143.4952,5558,3.4701,6279.5527,5199,0.1891,12139.5535,5161,1.3328,1748.0164,5115,0.2831,5856.4777,4900,0.4874,1194.4470,
    4104,5.3682,8429.2413,4094,2.3985,19651.0485,3920,6.1683,10447.3878,3677,6.0413,10213.2855,3660,2.5696,1059.3819,3595,
    1.7088,2352.8662,3557,1.7760,6812.7668,3329,0.5931,17789.8456,3041,0.4429,83996.8473,3005,2.7398,1349.8674,2535,3.1647,
    4690.4798,2474,0.2148,3.5904,2366,0.4847,8031.0923,2357,2.0653,3340.6124,2282,5.2220,4705.7323,2189,5.5559,553.5694,2142,
    1.4256,16730.4637,2109,4.1483,951.7184,2030,0.3713,283.8593,1992,5.2221,12168.0027,1986,5.7747,6309.3742,1912,3.8222,
    23581.2582,1889,5.3863,149854.4001,1790,2.2149,13367.9726,1748,4.5605,135.0651,1622,5.9884,11769.8537,1508,4.1957,
    6256.7775,1442,4.1932,242.7286,1435,3.7236,38.0277,1397,4.4014,6681.2249,1362,1.8893,7632.9433,1250,1.1305,5.5229,1205,
    2.6223,955.5997,1200,1.0035,632.7837,1129,0.1774,4164.3120,1083,0.3273,103.0928,1052,0.9387,11926.2544,1050,5.3591,
    1592.5960,1033,6.1998,6438.4962,1001,6.0291,5746.2713,980,0.999,11371.705,980,5.244,27511.468,938,2.624,5760.498,923,0.483,
    522.577,922,4.571,4292.331,905,5.337,6386.169,862,4.165,7058.598,841,3.299,7234.794,836,4.539,25132.303,813,6.112,4732.031,
    812,6.271,426.598,801,5.821,28.449,787,0.996,5643.179,776,2.957,23013.540,769,3.121,7238.676,758,3.974,11499.656,735,4.386,
    316.392,731,0.607,11513.883,719,3.998,74.782,706,0.323,263.084,676,5.911,90955.552,663,3.665,17298.182,653,5.791,18073.705,
    630,4.717,6836.645,615,1.458,233141.314,612,1.075,19804.827,596,3.321,6283.009,596,2.876,6283.143,555,2.452,12352.853,541,
    5.392,419.485,531,0.382,31441.678,519,4.065,6208.294,513,2.361,10973.556,494,5.737,9917.697,450,3.272,11015.106,449,3.653,
    206.186,447,2.064,7079.374,435,4.423,5216.580,421,1.906,245.832,413,0.921,3738.761,402,0.840,20.355,387,1.826,11856.219,
    379,2.344,3.881,374,2.954,3128.389,370,5.031,536.805,365,1.018,16200.773,365,1.083,88860.057,352,5.978,3894.182,352,2.056,
    244287.600,351,3.713,6290.189,340,1.106,14712.317,339,0.978,8635.942,339,3.202,5120.601,333,0.837,6496.375,325,3.479,
    6133.513,316,5.089,21228.392,316,1.328,10873.986,309,3.646,10.637,303,1.802,35371.887,296,3.397,9225.539,288,6.026,
    154717.610,281,2.585,14314.168,262,3.856,266.607,262,2.579,22483.849,257,1.561,23543.231,255,3.949,1990.745,251,3.744,
    10575.407,240,1.161,10984.192,238,0.106,7.046,236,4.272,6040.347,234,3.577,10969.965,211,3.714,65147.620,210,0.754,
    13521.751,207,4.228,5650.292,202,0.814,170.673,201,4.629,6037.244,200,0.381,6172.870,199,3.933,6206.810,199,5.197,6262.300,
    197,1.046,18209.330,195,1.070,5230.807,195,4.869,36.028,194,4.313,6244.943,192,1.229,709.933,192,5.595,6282.096,192,0.602,
    6284.056,189,3.744,23.878,188,1.904,15.252,188,0.867,22003.915,182,3.681,15110.466,181,0.491,1.484,179,3.222,39302.097,179,
    1.259,12559.038,
    {L1}
    62833196674749,0.000000000000,0.000000000000,20605886,2.67823456,6283.07584999,430343,2.635127,12566.151700,42526,1.59047,
    3.52312,11926,5.79557,26.29832,10898,2.96618,1577.34354,9348,2.5921,18849.2275,7212,1.1385,529.6910,6777,1.8747,398.1490,
    6733,4.4092,5507.5532,5903,2.8880,5223.6939,5598,2.1747,155.4204,4541,0.3980,796.2980,3637,0.4662,775.5226,2896,2.6471,
    7.1135,2084,5.3414,0.9803,1910,1.8463,5486.7778,1851,4.9686,213.2991,1729,2.9912,6275.9623,1623,0.0322,2544.3144,1583,
    1.4305,2146.1654,1462,1.2053,10977.0788,1246,2.8343,1748.0164,1188,3.2580,5088.6288,1181,5.2738,1194.4470,1151,2.0750,
    4694.0030,1064,0.7661,553.5694,997,1.303,6286.599,972,4.239,1349.867,945,2.700,242.729,858,5.645,951.718,758,5.301,
    2352.866,639,2.650,9437.763,610,4.666,4690.480,583,1.766,1059.382,531,0.909,3154.687,522,5.661,71430.696,520,1.854,801.821,
    504,1.425,6438.496,433,0.241,6812.767,426,0.774,10447.388,413,5.240,7084.897,374,2.001,8031.092,356,2.429,14143.495,350,
    4.800,6279.553,337,0.888,12036.461,337,3.862,1592.596,325,3.400,7632.943,322,0.616,8429.241,318,3.188,4705.732,297,6.070,
    4292.331,295,1.431,5746.271,290,2.325,20.355,275,0.935,5760.498,270,4.804,7234.794,253,6.223,6836.645,228,5.003,17789.846,
    225,5.672,11499.656,215,5.202,11513.883,208,3.955,10213.286,208,2.268,522.577,206,2.224,5856.478,206,2.550,25132.303,203,
    0.910,6256.778,189,0.532,3340.612,188,4.735,83996.847,179,1.474,4164.312,178,3.025,5.523,177,3.026,5753.385,159,4.637,
    3.286,157,6.124,5216.580,155,3.077,6681.225,154,4.200,13367.973,143,1.191,3894.182,138,3.093,135.065,136,4.245,426.598,134,
    5.765,6040.347,128,3.085,5643.179,127,2.092,6290.189,125,3.077,11926.254,125,3.445,536.805,114,3.244,12168.003,112,2.318,
    16730.464,111,3.901,11506.770,111,5.320,23.878,105,3.750,7860.419,103,2.447,1990.745,96,0.82,3.88,96,4.08,6127.66,91,5.42,
    206.19,91,0.42,7079.37,88,5.17,11790.63,81,0.34,9917.70,80,3.89,10973.56,78,2.40,1589.07,78,2.58,11371.70,77,3.98,955.60,
    77,3.36,36.03,76,1.30,103.09,75,5.18,10969.97,75,4.96,6496.37,73,5.21,38.03,72,2.65,6309.37,70,5.61,3738.76,69,2.60,
    3496.03,69,0.39,15.25,69,2.78,20.78,65,1.13,7058.60,64,4.28,28.45,61,5.63,10984.19,60,0.73,419.48,60,5.28,10575.41,58,5.55,
    17298.18,58,3.19,4732.03,
    {L2}
    5291887,0.0000000,0.0000000,871984,1.072097,6283.075850,30913,0.86729,12566.15170,2734,0.0530,3.5231,1633,5.1883,26.2983,
    1575,3.6846,155.4204,954,0.757,18849.228,894,2.057,77713.771,695,0.827,775.523,506,4.663,1577.344,406,1.031,7.114,381,
    3.441,5573.143,346,5.141,796.298,317,6.053,5507.553,302,1.192,242.729,289,6.117,529.691,271,0.306,398.149,254,2.280,
    553.569,237,4.381,5223.694,208,3.754,0.980,168,0.902,951.718,153,5.759,1349.867,145,4.364,1748.016,134,3.721,1194.447,125,
    2.948,6438.496,122,2.973,2146.165,110,1.271,161000.686,104,0.604,3154.687,100,5.986,6286.599,92,4.80,5088.63,89,5.23,
    7084.90,83,3.31,213.30,76,3.42,5486.78,71,6.19,4690.48,68,3.43,4694.00,65,1.60,2544.31,64,1.98,801.82,61,2.48,10977.08,50,
    1.44,6836.65,49,2.34,1592.60,46,1.31,4292.33,46,3.81,149854.40,43,0.04,7234.79,40,4.94,7632.94,39,1.57,71430.70,38,3.17,
    6309.37,35,0.99,6040.35,35,0.67,1059.38,31,3.18,2352.87,31,3.55,8031.09,30,1.92,10447.39,30,2.52,6127.66,28,4.42,9437.76,
    28,2.71,3894.18,27,0.67,25132.30,26,5.27,6812.77,25,0.55,6279.55,23,1.38,4705.73,22,0.64,6256.78,20,6.07,640.88,
    {L3}
    28923,5.84384,6283.07585,3496,0.0000,0.0000,1682,5.4877,12566.1517,296,5.196,155.420,129,4.722,3.523,71,5.30,18849.23,64,
    5.97,242.73,40,3.79,553.57,
    {L4}
    11408,3.14159,0.00000,772,4.134,6283.076,77,3.84,12566.15,42,0.42,155.42,
    {L5}
    88,3.14,0.00,17,2.77,6283.08,5,2.01,155.42,3,2.21,12566.15,
    {B0}
    27962,3.19870,84334.66158,10164,5.42249,5507.55324,8045,3.8801,5223.6939,4381,3.7044,2352.8662,3193,4.0003,1577.3435,2272,
    3.9847,1047.7473,1814,4.9837,6283.0758,1639,3.5646,5856.4777,1444,3.7028,9437.7629,1430,3.4112,10213.2855,1125,4.8282,
    14143.4952,1090,2.0857,6812.7668,1037,4.0566,71092.8814,971,3.473,4694.003,915,1.142,6620.890,878,4.440,5753.385,837,
    4.993,7084.897,770,5.554,167621.576,719,3.602,529.691,692,4.326,6275.962,558,4.410,7860.419,529,2.484,4705.732,521,6.250,
    18073.705,
    {B1}
    903,3.897,5507.553,618,1.730,5223.694,380,5.244,2352.866,
    {B2}
    166,1.627,84334.662,

    {R0}
    10001398880,0.00000000000,0.00000000000,167069963,3.098463508,6283.075849991,1395602,3.0552461,12566.1517000,308372,
    5.198467,77713.771468,162846,1.173877,5753.384885,157557,2.846852,7860.419392,92480,5.45292,11506.76977,54244,4.56409,
    3930.20970,47211,3.66100,5884.92685,34598,0.96369,5507.55324,32878,5.89984,5223.69392,30678,0.29867,5573.14280,24319,
    4.27350,11790.62909,21183,5.84715,1577.34354,18575,5.02194,10977.07880,17484,3.01194,18849.22755,10984,5.05511,5486.77784,
    9832,0.8868,6069.7768,8650,5.6896,15720.8388,8583,1.2708,161000.6857,6490,0.2725,17260.1547,6292,0.9218,529.6910,5706,
    2.0137,83996.8473,5574,5.2416,71430.6956,4938,3.2450,2544.3144,4696,2.5781,775.5226,4466,5.5372,9437.7629,4252,6.0111,
    6275.9623,3897,5.3607,4694.0030,3825,2.3926,8827.3903,3749,0.8295,19651.0485,3696,4.9011,12139.5535,3566,1.6747,12036.4607,
    3454,1.8427,2942.4634,3319,0.2437,7084.8968,3192,0.1837,5088.6288,3185,1.7778,398.1490,2846,1.2134,6286.5990,2779,1.8993,
    6279.5527,2628,4.5890,10447.3878,2460,3.7866,8429.2413,2393,4.9960,5856.4777,2359,0.2687,796.2980,2329,2.8078,14143.4952,
    2210,1.9500,3154.6871,2035,4.6527,2146.1654,1951,5.3823,2352.8662,1883,0.6731,149854.4001,1833,2.2535,23581.2582,1796,
    0.1987,6812.7668,1731,6.1520,16730.4637,1717,4.4332,10213.2855,1619,5.2316,17789.8456,1381,5.1896,8031.0923,1364,3.6852,
    4705.7323,1314,0.6529,13367.9726,1041,4.3329,11769.8537,1017,1.5939,4690.4798,998,4.201,6309.374,966,3.676,27511.468,874,
    6.064,1748.016,779,3.674,12168.003,771,0.312,7632.943,756,2.626,6256.778,746,5.648,11926.254,693,2.924,6681.225,680,1.423,
    23013.540,674,0.563,3340.612,663,5.661,11371.705,659,3.136,801.821,648,2.650,19804.827,615,3.029,233141.314,612,5.134,
    1194.447,563,4.341,90955.552,552,2.091,17298.182,534,5.100,31441.678,531,2.407,11499.656,523,4.624,6438.496,513,5.324,
    11513.883,477,0.256,11856.219,461,1.722,7234.794,458,3.766,6386.169,458,4.466,5746.271,423,1.055,5760.498,422,1.557,
    7238.676,415,2.599,7058.598,401,3.030,1059.382,397,1.201,1349.867,379,4.907,4164.312,360,5.707,5643.179,352,3.626,
    244287.600,348,0.761,10973.556,342,3.001,4292.331,336,4.546,4732.031,334,3.138,6836.645,324,4.164,9917.697,316,1.691,
    11015.106,307,0.238,35371.887,298,1.306,6283.143,298,1.750,6283.009,293,5.738,16200.773,286,5.928,14712.317,281,3.515,
    21228.392,280,5.663,8635.942,277,0.513,26.298,268,4.207,18073.705,266,0.900,12352.853,260,2.962,25132.303,255,2.477,
    6208.294,242,2.800,709.933,231,1.054,22483.849,229,1.070,14314.168,216,1.314,154717.610,215,6.038,10873.986,200,0.561,
    7079.374,198,2.614,951.718,197,4.369,167283.762,186,2.861,5216.580,183,1.660,39302.097,183,5.912,3738.761,175,2.145,
    6290.189,173,2.168,10575.407,171,3.702,1592.596,171,1.343,3128.389,164,5.550,6496.375,164,5.856,10984.192,161,1.998,
    10969.965,161,1.909,6133.513,157,4.955,25158.602,154,6.216,23543.231,153,5.357,13521.751,150,5.770,18209.330,150,5.439,
    155.420,139,1.778,9225.539,139,1.626,5120.601,128,2.460,13916.019,123,0.717,143571.324,122,2.654,88860.057,121,4.414,
    3894.182,121,1.192,3.523,120,4.030,553.569,119,1.513,17654.781,117,3.117,14945.316,113,2.698,6040.347,110,3.085,43232.307,
    109,0.998,955.600,108,2.939,17256.632,107,5.285,65147.620,103,0.139,11712.955,103,5.850,213.299,102,3.046,6037.244,101,
    2.842,8662.240,100,3.626,6262.300,98,2.36,6206.81,98,5.11,6172.87,98,2.00,15110.47,97,2.67,5650.29,97,2.75,6244.94,96,4.02,
    6282.10,96,5.31,6284.06,92,0.10,29088.81,85,3.26,20426.57,84,2.60,28766.92,81,3.58,10177.26,80,5.81,5230.81,78,2.53,
    16496.36,77,4.06,6127.66,73,0.04,5481.25,72,5.96,12559.04,72,5.92,4136.91,71,5.49,22003.91,70,3.41,7.11,69,0.62,11403.68,
    69,3.90,1589.07,69,1.96,12416.59,69,4.51,426.60,67,1.61,11087.29,66,4.50,47162.52,66,5.08,283.86,66,4.32,16858.48,65,1.04,
    6062.66,64,1.59,18319.54,63,5.70,45892.73,63,4.60,66567.49,63,3.82,13517.87,62,2.62,11190.38,61,1.54,33019.02,60,5.58,
    10344.30,60,5.38,316428.23,60,5.78,632.78,59,6.12,9623.69,57,0.16,17267.27,57,3.86,6076.89,57,1.98,7668.64,56,4.78,
    20199.09,55,4.56,18875.53,55,3.51,17253.04,54,3.07,226858.24,54,4.83,18422.63,53,5.02,12132.44,52,3.63,5333.90,52,0.97,
    155427.54,51,3.36,20597.24,50,0.99,11609.86,50,2.21,1990.75,48,1.62,12146.67,48,1.17,12569.67,47,4.62,5436.99,47,1.81,
    12562.63,47,0.59,21954.16,47,0.76,7342.46,46,0.27,4590.91,46,3.77,156137.48,45,5.66,10454.50,44,5.84,3496.03,43,0.24,
    17996.03,41,5.93,51092.73,41,4.21,12592.45,40,5.14,1551.05,40,5.28,15671.08,39,3.69,18052.93,39,4.94,24356.78,38,2.72,
    11933.37,38,5.23,7477.52,38,4.99,9779.11,37,3.70,9388.01,37,4.44,4535.06,36,2.16,28237.23,36,2.54,242.73,36,0.22,5429.88,
    35,6.15,19800.95,35,2.92,36949.23,34,5.63,2379.16,34,5.73,16460.33,34,5.11,5849.36,33,6.19,6268.85,
    {R1}
    10301861,1.10748970,6283.07584999,172124,1.064423,12566.151700,70222,3.14159,0.00000,3235,1.0217,18849.2275,3080,2.8435,
    5507.5532,2497,1.3191,5223.6939,1849,1.4243,1577.3435,1008,5.9138,10977.0788,865,1.420,6275.962,863,0.271,5486.778,507,
    1.686,5088.629,499,6.014,6286.599,467,5.987,529.691,440,0.518,4694.003,410,1.084,9437.763,387,4.750,2544.314,375,5.071,
    796.298,352,0.023,83996.847,344,0.949,71430.696,341,5.412,775.523,322,6.156,2146.165,286,5.484,10447.388,284,3.420,
    2352.866,255,6.132,6438.496,252,0.243,398.149,243,3.092,4690.480,225,3.689,7084.897,220,4.952,6812.767,219,0.420,8031.092,
    209,1.282,1748.016,193,5.314,8429.241,185,1.820,7632.943,175,3.229,6279.553,173,1.537,4705.732,158,4.097,11499.656,158,
    5.539,3154.687,150,3.633,11513.883,148,3.222,7234.794,147,3.653,1194.447,144,0.817,14143.495,135,6.151,5746.271,134,4.644,
    6836.645,128,2.693,1349.867,123,5.650,5760.498,118,2.577,13367.973,113,3.357,17789.846,110,4.497,4292.331,108,5.828,
    12036.461,102,5.621,6256.778,99,1.14,1059.38,98,0.66,5856.48,93,2.32,10213.29,92,0.77,16730.46,88,1.50,11926.25,86,1.42,
    5753.38,85,0.66,155.42,81,1.64,6681.22,80,4.11,951.72,66,4.55,5216.58,65,0.98,25132.30,64,4.19,6040.35,64,0.52,6290.19,63,
    1.51,5643.18,59,6.18,4164.31,57,2.30,10973.56,55,2.32,11506.77,55,2.20,1592.60,55,5.27,3340.61,54,5.54,553.57,53,5.04,
    9917.70,53,0.92,11371.70,52,3.98,17298.18,52,3.60,10969.97,49,5.91,3894.18,49,2.51,6127.66,48,1.67,12168.00,46,0.31,801.82,
    42,3.70,10575.41,42,4.05,10984.19,40,2.17,7860.42,40,4.17,26.30,38,5.82,7058.60,37,3.39,6496.37,36,1.08,6309.37,36,5.34,
    7079.37,34,3.62,11790.63,32,0.32,16200.77,31,4.24,3738.76,29,4.55,11856.22,29,1.26,8635.94,27,3.45,5884.93,26,5.08,
    10177.26,26,5.38,21228.39,24,2.26,11712.96,24,1.05,242.73,24,5.59,6069.78,23,3.63,6284.06,23,1.64,4732.03,22,3.46,213.30,
    21,1.05,3496.03,21,3.92,13916.02,21,4.01,5230.81,20,5.16,12352.85,20,0.69,1990.75,19,2.73,6062.66,19,5.01,11015.11,18,6.04,
    6283.01,18,2.85,7238.68,18,5.60,6283.14,18,5.16,17253.04,18,2.54,14314.17,17,1.58,7.11,17,0.98,3930.21,17,4.75,17267.27,16,
    2.19,6076.89,16,2.19,18073.70,16,6.12,3.52,16,4.61,9623.69,16,3.40,16496.36,15,0.19,9779.11,15,5.30,13517.87,15,4.26,
    3128.39,15,0.81,709.93,14,0.50,25158.60,14,4.38,4136.91,13,0.98,65147.62,13,3.31,154717.61,13,2.11,1589.07,13,1.92,
    22483.85,12,6.03,9225.54,12,1.53,12559.04,12,5.82,6282.10,12,5.61,5642.20,12,2.38,167283.76,12,0.39,12132.44,12,3.98,
    4686.89,12,5.81,12569.67,12,0.56,5849.36,11,0.45,6172.87,11,5.80,16858.48,11,6.22,12146.67,11,2.27,5429.88,
    {R2}
    435939,5.784551,6283.075850,12363,5.57935,12566.15170,1234,3.1416,0.0000,879,3.628,77713.771,569,1.870,5573.143,330,5.470,
    18849.228,147,4.480,5507.553,110,2.842,161000.686,101,2.815,5223.694,85,3.11,1577.34,65,5.47,775.52,61,1.38,6438.50,50,
    4.42,6286.60,47,3.66,7084.90,46,5.39,149854.40,42,0.90,10977.08,40,3.20,5088.63,35,1.81,5486.78,32,5.35,3154.69,30,3.52,
    796.30,29,4.62,4690.48,28,1.84,4694.00,27,3.14,71430.70,27,6.17,6836.65,26,1.42,2146.17,25,2.81,1748.02,24,2.18,155.42,23,
    4.76,7234.79,21,3.38,7632.94,21,0.22,4705.73,20,4.22,1349.87,20,2.01,1194.45,20,4.58,529.69,19,1.59,6309.37,18,5.70,
    6040.35,18,6.03,4292.33,17,2.90,9437.76,17,2.00,8031.09,17,5.78,83996.85,16,0.05,2544.31,15,0.95,6127.66,14,0.36,10447.39,
    14,1.48,2352.87,13,0.77,553.57,13,5.48,951.72,13,5.27,6279.55,13,3.76,6812.77,11,5.41,6256.78,10,0.68,1592.60,10,4.95,
    398.15,10,1.15,3894.18,10,5.20,244287.60,10,1.94,11856.22,9,5.39,25132.30,8,6.18,1059.38,8,0.69,8429.24,8,5.85,242.73,7,
    5.26,14143.50,7,0.52,801.82,6,2.24,8635.94,6,4.00,13367.97,6,2.77,90955.55,6,5.17,7058.60,5,1.46,233141.31,5,4.13,7860.42,
    5,3.91,26.30,5,3.89,12036.46,5,5.58,6290.19,5,5.54,1990.75,5,0.83,11506.77,5,6.22,6681.22,4,5.26,10575.41,4,1.91,7477.52,4,
    0.43,10213.29,4,1.09,709.93,4,5.09,11015.11,4,4.22,88860.06,4,3.57,7079.37,4,1.98,6284.06,4,3.93,10973.56,4,6.18,9917.70,4,
    0.36,10177.26,4,2.75,3738.76,4,3.33,5643.18,4,5.36,25158.60,
    {R3}
    14459,4.27319,6283.07585,673,3.917,12566.152,77,0.00,0.00,25,3.73,18849.23,4,2.80,6286.60,
    {R4}
    386,2.564,6283.076,31,2.27,12566.15,5,3.44,5573.14,2,2.05,18849.23,1,2.06,77713.77,1,4.41,161000.69,1,3.82,149854.40,1,
    4.08,6127.66,1,5.26,6438.50,
    {R5}
    9,1.22,6283.08,1,0.66,12566.15
  );

// ������ݺͼ��ٶȽ��ж�����������
function DiffTimeExt(Y, JSD: Extended): Extended;
var
  DY: Extended;
begin
  DY := (Y - 1820) / 100;
  Result := JSD * DY * DY - 20;
end;

// ��������ʱ��ԭ��ʱ֮�������
function CalcDiffTime(Y: Extended): Extended;
var
  Y0, T0, V, DV, Acc, T1, T2, T3: Extended;
  I: Integer;
begin
  Y0 := CN_DT_AT[High(CN_DT_AT) - 1]; // �������һ��
  T0 := CN_DT_AT[High(CN_DT_AT)];     // �������һ��� deltatT
  if Y >= Y0 then
  begin
    Acc := 31; // y0 ��֮��ļ��ٶȹ��ơ���ʿ������ 31��NASA ��վ 32��skmap �� 29
    if Y > Y0 + 100 then
    begin
      Result := DiffTimeExt(Y, Acc);
      Exit;
    end;
    V := DiffTimeExt(Y, Acc);         // ������������
    DV := DiffTimeExt(Y0, Acc) - T0;  // ye ��Ķ��������� te �Ĳ�
    Result := V - DV * (Y0 + 100 - Y) / 100;
    Exit;
  end;

  I := 0;
  while I < High(CN_DT_AT) do
  begin
    if Y < CN_DT_AT[I + 5] then
      Break;
    I := I + 5;
  end;

  T1 := (Y - CN_DT_AT[I]) / (CN_DT_AT[I + 5] - CN_DT_AT[I]) * 10;
  T2 := T1 * T1;
  T3 := T2 * T1;
  Result := CN_DT_AT[I + 1] + CN_DT_AT[I + 2] * T1 +CN_DT_AT[I + 3] * T2
    + CN_DT_AT[I + 4] * T3;
end;

// ���������գ�J2000 ���㣩������ TD-UT����λ���գ�
function dt_T(T: Extended): Extended;
begin
  Result := CalcDiffTime(T / 365.2425 + 2000) / 86400.0;
end;

// ̫�����вT ��������
function SolarAberration(T: Extended): Extended;
var
  V, E: Extended;
begin
  V := -0.043126 + 628.301955 * T - 0.000002732 * T * T;     // ƽ�����
  E := 0.016708634 - 0.000042037 * T - 0.0000001267 * T * T;
  Result := (-20.49552 * (1 + E * Cos(V))) / RAD;            // �ƾ����в�
end;

// �ƾ��¶�����
function EclipticLongitudeNutation(T: Extended): Extended;
var
  I: Integer;
  A, T2, DL: Extended;
begin
  T2 := T * T;
  DL := 0;

  I := 0;
  while I < High(nutB) do
  begin
    if I = 0 then
      A := -1.742 * T
    else
      A := 0;

    DL := DL + (nutB[I + 3] + A) * Sin(nutB[I] + nutB[I + 1] * T + nutB[I + 2] * T2);
    I := I + 5;
  end;
  Result := DL / (100 * RAD);
end;

// ��������㣬t ������������n ��������
function CalcXingLi0(t, n: Extended): Extended;
var
  v, tn, c, NB, t2, t3: Extended;
  I, J, pn, n1, n2, n0, NZ: Integer;
begin
  t := t / 10;
  v := 0;
  tn := 1;
  pn := 1;

  NZ := Trunc(XL0[pn + 1] - XL0[pn]);

  for I := 0 to 5 do
  begin
    n1 := Trunc(XL0[pn + I]);
    n2 := Trunc(XL0[pn + I + 1]);
    n0 := n2 - n1;

    if n0 = 0 then
      Continue;

    if n < 0 then
      NB := n2
    else
    begin
      NB := Trunc(3 * n * n0 / NZ + 0.5) + n1;
      if I <> 0 then
        NB := NB + 3;
      if NB > n2 then
        NB := n2;
    end;

    c := 0;
    J := n1;
    while J < NB do
    begin
      c := c + XL0[J] * Cos(XL0[J + 1] + t * XL0[J + 2]);
      J := J + 3;
    end;

    v := v + c * tn;
    tn := tn * t;
  end;

  v := v / XL0[0];

  t2 := t * t;
  t3 := t2 * t; // ǧ�����ĸ��η�

  v := v + (-0.0728 -2.7702 * t -1.1019 * t2 -0.0996 * t3) / RAD;
  Result := v;
end;

// ����̫���ӻƾ����ڲ�����������������������������еĵ��򾭶ȼ��㣬������������ȡ���������� Date �ֵ�ƾ�
function SolarApparentLongitude(T, N: Extended): Extended;
begin
  Result := CalcXingLi0(T, N) + EclipticLongitudeNutation(T) + SolarAberration(T) + CN_PI; // ע��������¶�����ܺ�ʱ
end;

// �����ٶȼ��㣬T �������������С�����֮ 3
function EearthVelocity(T: Extended): Extended;
var
  F: Extended;
begin
  F := 628.307585 * T;
  Result := 628.332 + 21 * Sin(1.527 + F) + 0.44 * Sin(1.48 + F * 2)
    + 0.129 * Sin(5.82 + F) * T + 0.00055 * Sin(4.21 + F) * T * T;
end;

// ���ݼ������̫���ӻƾ�Ҳ���ǽ�����Ӧ�Ƕȷ�������ʱ�䣬���Ϊ��������
function GetDateTimeFromSolarApparentLongitude(W: Extended): Extended;
var
  T, V: Extended;
begin
  V := 628.3319653318;
  T := (W - 1.75347 - CN_PI) / V;
  V := EearthVelocity(T); // v �ľ��� 0.03%
  T := T + ( W - SolarApparentLongitude(T, 10) ) / V;
  V := EearthVelocity(T); // ����һ�� V ��������߾��ȣ�����Ҳ����
  T := T + ( W - SolarApparentLongitude(T, -1) ) / V;
  Result := T;
end;

// �����㷨֮���ĳ�������ڵĵ� N �������������������1-24����ӦС������������������Ϊ 0
function GetJieQiDayTimeFromYear2(AYear, N: Integer): Extended;
var
  Y: Integer;
  T, JD, JD0: Extended;
begin
  Y := AYear - 2000;
  if Y = -2000 then
    Dec(Y);

  T := GetDateTimeFromSolarApparentLongitude((Y + (N - 6) * 15 / 360 + 1) * 2 * CN_PI);
  // �����Ƕ�
  JD := T * 36525 + J2000 + 8/24 - dt_T(T * 36525);

  Dec(AYear);
  if AYear = 0 then
    Dec(AYear);
  JD0 := GetJulianDate(AYear, 12, 31) - 0.5;
  Result := JD - JD0;
end;

// =========================== ������ȷ�㷨���� ================================

// ���ĳ������ĵ� N �������Ľ�������ʱ�֣�0-23����ӦС��������
function GetJieQiInAYear(AYear, N: Integer; out AMonth: Integer;
  out ADay: Integer; out AHour: Integer; out AMinitue: Integer; out ASecond: Integer): Boolean;
var
  Days: Extended;
  I, Day: Integer;
begin
  Result := N in [0..23];
  if Result then
  begin
    Days := GetJieQiDayTimeFromYear2(AYear, N + 1);
    for I := 1 to 12 do
    begin
      Day := GetMonthDays(AYear, I);
      if Days > Day then
        Days := Days - Day
      else
      begin
        AMonth := I;
        Break;
      end;
    end;
    ADay := Floor(Days);

    Days := Days - ADay;
    AHour := Floor(Days * 24);

    Days := Days * 24 - AHour;
    AMinitue := Floor(Days * 60);

    Days := Days * 60 - AMinitue;
    ASecond := Round(Days * 60);

    // �����ǡ�õ���60�����Ҫ��һ�����ǡ�õ��� 60����Сʱ��Ҫ��һ�����Сʱǡ�õ��� 24��������Ҫ��һ
    if ASecond >= 60 then
    begin
      Dec(ASecond, 60);
      Inc(AMinitue);

      if AMinitue >= 60 then
      begin
        Dec(AMinitue, 60);
        Inc(AHour);

        if AHour >= 24 then
        begin
          Dec(AHour, 24);
          Inc(ADay);
        end;

        // ���������µף����һ�㲻�ÿ���������һ���·ݸı�����
      end;
    end;
  end
  else
  begin
    AMonth := 0;
    ADay := 0;
    AHour := 0;
    AMinitue := 0;
  end;
end;

// ��ù����������Ǳ����ʲô������0-23����Ӧ�������󺮣����򷵻� -1
function GetJieQiFromDay(AYear, AMonth, ADay: Integer): Integer;
var
  Month, Day, Idx, DummyHour, DummyMinute, DummySec: Integer;
begin
  Result := -1;

  // ÿ����������������������ڴ��¶�Ӧ�����پ�ȷ���㣬���Ż�����
  Idx := (AMonth - 1) * 2;
  if ADay >= 15 then
    Inc(Idx);

  if GetJieQiInAYear(AYear, Idx, Month, Day, DummyHour, DummyMinute, DummySec) then
  begin
    if (AMonth = Month) and (ADay = Day) then
    begin
      // ��ʱ I ��ʾ 0 ��С��
      Result := Idx - 2;
      // ת���� 0 ������
      if Result < 0 then
        Inc(Result, 24);
      Exit;
    end;
  end;
end;

// ��ù����������Ǳ����ʲô�����Լ�����ʱ�̣�0-23����Ӧ�������󺮣����򷵻� -1
function GetJieQiTimeFromDay(AYear, AMonth, ADay: Integer; out AHour: Integer;
  out AMinitue: Integer; out ASecond: Integer): Integer;
var
  Month, Day, Idx: Integer;
begin
  Result := -1;

  // ÿ����������������������ڴ��¶�Ӧ�����پ�ȷ���㣬���Ż�����
  Idx := (AMonth - 1) * 2;
  if ADay >= 15 then
    Inc(Idx);

  if GetJieQiInAYear(AYear, Idx, Month, Day, AHour, AMinitue, ASecond) then
  begin
    if (AMonth = Month) and (ADay = Day) then
    begin
      // ��ʱ I ��ʾ 0 ��С��
      Result := Idx - 2;
      // ת���� 0 ������
      if Result < 0 then
        Inc(Result, 24);
      Exit;
    end;
  end;
  AHour := -1;
  AMinitue := -1;
  ASecond := -1;
end;

// ���ĳ����ʱ����ɵ�֧��0-59 ��Ӧ ���ӵ��ﺥ
function GetGanZhiFromHour(AYear, AMonth, ADay, AHour: Integer): Integer;
var
  Gan, Zhi, DummyZhi: Integer;
begin
  AHour := AHour mod 24;
  ExtractGanZhi(GetGanZhiFromDay(AYear, AMonth, ADay), Gan, DummyZhi);

  // Zhi��ʱ����(0-11)Ҳ����֧��
  if AHour = 23 then
  begin
    // ������ʱ
    Gan := (Gan + 1) mod 10;
    Zhi := 0;
  end
  else
  begin
    Inc(AHour);
    Zhi := AHour div 2;
  end;

  // Gan ��ʱ�Ǳ��ո��������ݹ�����ɱ�����ʱ������
  if Gan >= 5 then
    Dec(Gan, 5);
  Gan := 2 * Gan;

  // �����ʱ������
  Gan := (Gan + Zhi) mod 10;
  Result := CombineGanZhi(Gan, Zhi);
end;

// ���ĳ�����յ���ɵ�֧��0-59 ��Ӧ ���ӵ��ﺥ
function GetGanZhiFromDay(AllDays: Integer): Integer;
begin
  Result := (AllDays + 12) mod 60;
  if Result < 0 then
    Inc(Result, 60);
end;

function GetGanZhiFromDay(AYear, AMonth, ADay: Integer): Integer;
begin
  Result := GetGanZhiFromDay(GetAllDays(AYear, AMonth, ADay));
end;

// ���ĳ�����յ���ɵ�֧��0-59 ��Ӧ ���ӵ��ﺥ��Сʱ���������ж� 23 Сʱ���Ǵ���}
function GetGanZhiFromDay(AYear, AMonth, ADay, AHour: Integer): Integer;
begin
  AHour := AHour mod 24;
  if AHour >= 23 then
    Result := GetGanZhiFromDay(GetAllDays(AYear, AMonth, ADay) + 1)
  else
    Result := GetGanZhiFromDay(GetAllDays(AYear, AMonth, ADay));
end;

// ���ĳ�����µ���ɵ�֧��0-59 ��Ӧ ���ӵ��ﺥ
function GetGanZhiFromMonth(AYear, AMonth, ADay: Integer): Integer;
begin
  Result := GetGanZhiFromMonth(AYear, AMonth, ADay, 0);
end;

// ���ĳ�����µ���ɵ�֧��0-59 ��Ӧ ���ӵ��ﺥ
function GetGanZhiFromMonth(AYear, AMonth, ADay, AHour: Integer): Integer;
var
  Gan, DummyZhi, M: Integer;
begin
  // ��Ҫ�ȸ��ݽ��������·����Լ������Ϊ��׼��֧����
  AdjustYearMonthToGanZhi(AYear, AMonth, ADay, AHour);

  Result := -1;
  ExtractGanZhi(GetGanZhiFromYear(AYear), Gan, DummyZhi);
  case Gan of // ���ݿھ��ӱ���������㱾�����£�����֮�����ڵ��£�һ���Ƕ��£��ĸ���
    0,5: // �׼� �����ף�
      Result := 2;
    1,6: // �Ҹ� ��Ϊͷ��
      Result := 4;
    2,7: // ���� Ѱ����
      Result := 6;
    3,8: // ���� ��λ����
      Result := 8;
    4,9: // ��� �׺���
      Result := 0;
  end;

  M := AMonth - 1;       // �����֧��Ԫ���·� AMonth ������������� 1 ���·ݲ�
  if M < 0 then
    M := M + 10;
  Inc(Result, M mod 10); // ���㱾�¸���

  if Result >= 10 then
    Result := Result mod 10;

  // ���֧��������֮������ڵı���Ϊ����1 ����Ϊ 2�������֧�·ݼ� 1 ��Ϊ��֧
  Result := CombineGanZhi(Result, (AMonth + 1) mod 12);
end;

// ���ĳ��/ũ����ĸ�֧��0-59 ��Ӧ ���ӵ��ﺥ
function GetGanZhiFromYear(AYear: Integer): Integer;
begin
  if AYear = 0 then
    raise ECnDateTimeException.Create(SCnErrorYearIsInvalid);

  if AYear > 0 then
    Result := (AYear - 4) mod 60
  else // ��Ҫ�����жϹ�Ԫǰ��ԭ����û�й�Ԫ 0 ��
    Result := (AYear - 3) mod 60;

  if Result < 0 then
    Inc(Result, 60);
end;

// ���ݹ��������ջ��ĳ���������ɵ�֧��������Ϊ��ֽ磬0-59 ��Ӧ ���ӵ��ﺥ
function GetGanZhiFromYear(AYear, AMonth, ADay: Integer): Integer; overload;
begin
  ValidDate(AYear, AMonth, ADay);

  // ����������ǰ������ǰһ�ꡣ������������һ��
  if GetDayFromYearBegin(AYear, AMonth, ADay) < Floor(GetJieQiDayTimeFromYear(AYear, 3)) then
  begin
    Dec(AYear);
    if AYear = 0 then // û�й�Ԫ 0 ��
      AYear := -1;
  end;
  Result := GetGanZhiFromYear(AYear);
end;

// ���ݹ��������ջ��ĳ���������ɵ�֧��������Ϊ��ֽ磬��ȷ��Сʱ��0-59 ��Ӧ ���ӵ��ﺥ
function GetGanZhiFromYear(AYear, AMonth, ADay, AHour: Integer): Integer; overload;
begin
  ValidDate(AYear, AMonth, ADay);
  ValidTime(AHour, 0, 0);

  // ����������ǰ������ǰһ�꣬��ȷ��Сʱ�жϡ�������������һ��
  if GetDayFromYearBegin(AYear, AMonth, ADay, AHour) < Floor(GetJieQiDayTimeFromYear(AYear, 3)) then
  begin
    Dec(AYear);
    if AYear = 0 then // û�й�Ԫ 0 ��
      AYear := -1;
  end;

  Result := GetGanZhiFromYear(AYear);
end;

// ���ĳ��/ũ�������ɣ�0-9 ��Ӧ �׵���
function GetGanFromYear(AYear: Integer): Integer;
begin
  if AYear = 0 then
    raise ECnDateTimeException.Create(SCnErrorYearIsInvalid);

  if AYear > 0 then
    Result := (AYear - 4) mod 10
  else // ��Ҫ�����жϹ�Ԫǰ��ԭ����û�й�Ԫ 0 ��
    Result := (AYear - 3) mod 10;

  if Result < 0 then
    Inc(Result, 10);
end;

// ���ĳ��/ũ����ĵ�֧��0-11 ��Ӧ �ӵ���
function GetZhiFromYear(AYear: Integer): Integer;
begin
  if AYear = 0 then
    raise ECnDateTimeException.Create(SCnErrorYearIsInvalid);

  if AYear > 0 then
    Result := (AYear - 4) mod 12
  else // ��Ҫ�����жϹ�Ԫǰ��ԭ����û�й�Ԫ 0 ��
    Result := (AYear - 3) mod 12;

  if Result < 0 then
    Inc(Result, 12);
end;

// ���ĳ��/ũ�������ФҲ���ǵ�֧��0-11 ��Ӧ ����
function GetShengXiaoFromYear(AYear: Integer): Integer;
begin
  Result := GetZhiFromYear(AYear);
end;

// ���ĳ�������յ�������0-11 ��Ӧ ����˫��}
function GetXingZuoFromMonthDay(AMonth, ADay: Integer): Integer;
const
  SCnXingZuoDays: array[0..11] of Integer =
    (120, 219, 321, 421, 521, 622, 723, 823, 923, 1023, 1123, 1222);
  // ÿ����������ʼ����
var
  I, Days: Integer;
begin
  Result := -1;
  Days := AMonth * 100 + ADay;

  for I := 0 to 11 do
  begin
    // �����ڵ�һ�������Ǳ�ƿ���ԵüӸ�ƫ��
    if Days < SCnXingZuoDays[I] then
    begin
      Result := (I + 9) mod 12;
      Exit;
    end
    else if (Days >= SCnXingZuoDays[I]) and (Days < SCnXingZuoDays[I + 1]) then
    begin
      Result := (I + 10) mod 12;
      Exit;
    end;
  end;
end;

// ���ĳ�������յ�ʮ������0-11 ��Ӧ ������
function Get12JianFromDay(AYear, AMonth, ADay: Integer): Integer;
var
  I, LiChun, JianStart, Days, AllDays, JieQi: Integer;
  DummyGan, Zhi: Integer;
begin
  Result := -1;

  // ʮ���������ڵ�֧����ת�����ڷ������Ľ���������ظ�ǰһ���
  // �������һ������Ϊ����
  JianStart := -1;
  LiChun := Floor(GetJieQiDayTimeFromYear(AYear, 3)); // ���������
  AllDays := GetAllDays(AYear, 1, 1) - 1;

  for I := LiChun + 1 to LiChun + 13 do
  begin
    ExtractGanZhi(GetGanZhiFromDay(AllDays + I), DummyGan, Zhi);

    // �õ�����֧�����ж��Ƿ���
    if Zhi = 2 then
    begin
      JianStart := I;
      Break;
    end;
  end;

  Days := GetDayFromYearBegin(AYear, AMonth, ADay);

  // �ҵ���������ĵ�һ������
  if JianStart > 0 then
  begin
    // ������������
    if JianStart = Days then
    begin
      Result := 0;
      Exit;
    end
    else
    begin
      Result := Days - JianStart; // �ȼ����ֵ���������� mod 12 ����

      if Days < JianStart then // ֮ǰ֮�����ֲ�ͬ���������
      begin
        for I := 3 downto 1 do
        begin
          if (I mod 2 = 0) then Continue; // ��������
          JieQi := Floor(GetJieQiDayTimeFromYear(AYear, I));
          if JieQi > Days then // �˽������ڴ����ں󣬱�ʾ֮�󵽽�����ʮ������ͣ��
            Inc(Result);
        end;
      end
      else
      begin
        for I := 4 to 24 do
        begin
          if (I mod 2 = 0) then Continue; // ��������
          JieQi := Floor(GetJieQiDayTimeFromYear(AYear, I));
          if JieQi <= Days then // �˽������ڴ�����ǰ����ʾ��ʮ������ͣ��
            Dec(Result);
        end;
      end;

      Result := Result mod 12;
      if Result < 0 then
        Inc(Result, 12);
    end;
  end;
end;

// ���ĳ�����յĶ�ʮ���ޣ�0-27 ��Ӧ �ǵ���
function Get28XiuFromDay(AYear, AMonth, ADay: Integer): Integer;
begin
  // +22 Դ�ڹ�Ԫ 1 �� 1 �� 0 ������
  Result := (GetAllDays(AYear, AMonth, ADay) + 22) mod 28;
  if Result < 0 then
    Inc(Result, 28);
end;

// ���ĳ�����յ�ũ����ʮ���ޣ�0-27 ��Ӧ�ǵ���
function GetLunar28XiuFromDay(AYear, AMonth, ADay: Integer): Integer;
var
  LY, LM, LD: Integer;
  LP: Boolean;
begin
  Result := -1;
  if GetLunarFromDay(AYear, AMonth, ADay, LY, LM, LD, LP) then
  begin
    // ת����ũ���������¡��ռ���
    if (LM in [1..12]) and (LD in [1..30]) then
      Result := SCnLunar28XiuNumber[LM, LD];
  end;
end;

// ���ĳ�����յ�̥��λ��0-59 ����̥��λ�ü�̥��λ���ַ���
function GetTaiShenStringFromDay(AYear, AMonth, ADay: Integer): string; overload;
var
  GanZhi: Integer;
begin
  GanZhi := GetGanZhiFromDay(AYear, AMonth, ADay);
  Result := SCnTaiShen1Array[GanZhi] + SCnTaiShen2Array[GanZhi];
end;

// ���ĳ�����յ�̥��λ��0-59 ����̥��λ����̥��λ�����ַ���
function GetTaiShenStringFromDay(AYear, AMonth, ADay: Integer;
  out TaiShen1: string; out TaiShen2: string): Boolean;
var
  GanZhi: Integer;
begin
  GanZhi := GetGanZhiFromDay(AYear, AMonth, ADay);
  TaiShen1 := SCnTaiShen1Array[GanZhi];
  TaiShen2 := SCnTaiShen2Array[GanZhi];
  Result := True;
end;

// ���Сʱʱ�̶�Ӧ��ʱ����0-11 ��Ӧ������
function GetShiChenFromHour(AHour: Integer): Integer;
begin
  Result := -1;
  if not (AHour in [0..23]) then
    Exit;

  if AHour = 23 then
  begin
    // ������ʱ
    Result := 0;
  end
  else
  begin
    Inc(AHour);
    Result := AHour div 2;
  end;
end;

// ��������Ϊ�磬����������������յ���������������������ĸ�֧�ȸ���ļ���
function AdjustYearToGanZhi(var AYear: Integer; AMonth: Integer;
  ADay: Integer; AHour: Integer): Boolean;
var
  Days: Extended;
begin
  Result := GetDateIsValid(AYear, AMonth, ADay);
  if not Result then
    Exit;

  Days := GetDayFromYearBegin(AYear, AMonth, ADay, AHour);

  // ������ļ�¼����Ϊ�����ɵ�֧������������Ϊ�ֽ�ģ�
  // �籾���Ǳ��������������ǰ��������ǰһ�ꡣ������������һ��
  if Days < Floor(GetJieQiDayTimeFromYear(AYear, 3)) then
  begin
    // ����Ҫ����Ϊǰһ��
    Dec(AYear);
  end;
end;

// �������������Ϊ�磬����������������յ���������·�������׼��֧����
function AdjustYearMonthToGanZhi(var AYear: Integer; var AMonth: Integer;
  ADay: Integer; AHour: Integer): Boolean;
var
  Days, JieQi: Extended;
begin
  Result := GetDateIsValid(AYear, AMonth, ADay);
  if not Result then
    Exit;

  Days := GetDayFromYearBegin(AYear, AMonth, ADay, AHour);

  JieQi := Floor(GetJieQiDayTimeFromYear(AYear, 3)); // 2 �µ�����
  if Days < JieQi then
  begin
    Dec(AYear);    // ����֮ǰ����ȥ�꣬��Ҫע�⹫����û�й�Ԫ 0 ��
    if AYear = 0 then
      Dec(AYear);

    JieQi := Floor(GetJieQiDayTimeFromYear(AYear, 1)); // 1 �µ�С��
    if Days < JieQi then
      AMonth := 11       // С��ǰ���֧�� 11 ��
    else
      AMonth := 12;      // С��������ǰ�߰���֧�� 12 ��
  end
  else
  begin
    // ���㱾��Ľ���������ǰһ��ģ�����������������������
    // ������������׽����ľ���ͷ���������ڵ��ڴ��գ�������������ϸ��£�����������������

    // ������ AMonth �ĵ�һ������������� 2 * AMonth - 1������µ�һ������������ 3
    if Days < Floor(GetJieQiDayTimeFromYear(AYear, 2 * AMonth - 1)) then
      Dec(AMonth, 2)
    else
      Dec(AMonth);
  end;
end;

// �����ֻ����Ԫ���ƣ�0-2
function Get3YuanFromNumber(A3Yuan: Integer): string;
begin
  Result := '';
  if (A3Yuan >= 0) and (A3Yuan < 3) then
    Result := SCn3YuanArray[A3Yuan];
end;

// �����ֻ�þ������ƣ�0-8
function Get9XingFromNumber(A9Xing: Integer): string;
begin
  Result := '';
  if (A9Xing >= 0) and (A9Xing < 9) then
    Result := SCn9XingArray[A9Xing];
end;

// �����ֻ���������ƣ�0-5
function Get6YaoFromNumber(A6Yao: Integer): string;
begin
  Result := '';
  if (A6Yao >= 0) and (A6Yao < 6) then
    Result := SCn6YaoArray[A6Yao];
end;

// ��ȡ��������������Ԫ��0-2 ��Ӧ��Ԫ��Ԫ��Ԫ
function Get3YuanFromYear(AYear, AMonth, ADay: Integer): Integer;
begin
  Result := -1;
  if AYear = 0 then
    Exit;

  AYear := GetYearSeperatedByLiChun(AYear, AMonth, ADay);
  if AYear < 0 then  // �����޹�Ԫ 0 ������
    Inc(AYear);

  // 1864 ����ĳһ����Ԫ֮ʼ
  AYear := (AYear - 1864) mod 180;
  if AYear < 0 then
    Inc(AYear, 180);

  if AYear in [0..59] then
    Result := 0
  else if AYear in [60..119] then
    Result := 1
  else
    Result := 2;
end;

// ��ȡ��������˾��ǣ�0-8 ��Ӧһ�׵�����
function GetYun9XingFromYear(AYear, AMonth, ADay: Integer): Integer;
begin
  Result := -1;
  if AYear = 0 then
    Exit;

  AYear := GetYearSeperatedByLiChun(AYear, AMonth, ADay);
  if AYear < 0 then  // �����޹�Ԫ 0 ������
    Inc(AYear);

  // 1864 ����ĳһ����ԪҲ���Ǿ���֮ʼ
  AYear := (AYear - 1864) mod 180;
  if AYear < 0 then
    Inc(AYear, 180);

  Result := AYear div 20;
end;

// ��ȡ�����������ǣ�0-8 ��Ӧһ�׵�����
function Get9XingFromYear(AYear, AMonth, ADay: Integer): Integer;
var
  Yuan: Integer;
begin
  Result := -1;
  ValidDate(AYear, AMonth, ADay);

  Yuan := Get3YuanFromYear(AYear, AMonth, ADay);
  AYear := GetYearSeperatedByLiChun(AYear, AMonth, ADay);

  AYear := (AYear - 1864) mod 60;
  if AYear < 0 then
    Inc(AYear, 60);

  case Yuan of
    0:       // ��Ԫ��һ��
      begin
        Result := 8 - ((AYear + 8) mod 9);
      end;
    1:       // ��Ԫ������
      begin
        Result := 8 - ((AYear + 5) mod 9);
      end;
    2:       // ��Ԫ���߳�
      begin
        Result := 8 - ((AYear + 2) mod 9);
      end;
  end;
end;

// ��ȡ�����µ��¾��ǣ�0-8 ��Ӧһ�׵�����
function Get9XingFromMonth(AYear, AMonth, ADay: Integer): Integer;
var
  Zhi: Integer;
begin
  Result := -1;
  if AdjustYearMonthToGanZhi(AYear, AMonth, ADay, 0) then
  begin
    // �õ������ָ�����Լ������ָ���º��ȡ���֧��
    // ע�������ߵ��Ǳ�׼��֧���꣬Ҳ������������������� 1��������·�������ŷ��Ͽھ�
    Zhi := GetZhiFromYear(AYear);
    case Zhi of
      0, 3, 6, 9:
        begin
          // ����î�ϰ˰���
          Result := 8 - (AMonth mod 9);
        end;
      2, 5, 8, 11:
        begin
          // �����Ⱥ�������
          Result := 8 - ((AMonth + 6) mod 9);
        end;
      1, 4, 7, 10:
        begin
          // �����δ�����
          Result := 8 - ((AMonth + 3) mod 9);
        end;
    end;
  end;
end;

// ��ȡ�����յ��վ��ǣ�0-8 ��Ӧһ�׵�����
function Get9XingFromDay(AYear, AMonth, ADay: Integer): Integer;
const
  JIEQI_SEQ: array[0..5] of Integer = (0, 4, 8, 12, 16, 20);
  // ��������һ�꣩����ˮ�����ꡢ���������˪����������
var
  I, PreYear, GanZhi, AllDays, Days: Integer;
  Matched: Boolean;
  JieQis: array[0..5] of Integer; // �����������ڣ���������������
  JiaZiQians: array[0..5] of Integer; // ��������ǰ�ĵ�һ�������յ����ڣ���������������
  JiaZiHous: array[0..5] of Integer;  // ����������ĵ�һ�������յ����ڣ���������������
begin
  Result := -1;
  if AYear = 0 then
    Exit;

  if AYear = 1 then
    PreYear := -1
  else
    PreYear := AYear - 1;

  for I := Low(JIEQI_SEQ) to High(JIEQI_SEQ) do
  begin
    if JIEQI_SEQ[I] > 0 then
    begin
      JieQis[I] := Floor(GetJieQiDayTimeFromYear(AYear, JIEQI_SEQ[I]));
      AllDays := GetAllDays(AYear, 1, 1) - 1;
    end
    else
    begin
      JieQis[I] := Floor(GetJieQiDayTimeFromYear(PreYear, JIEQI_SEQ[I] + 24));
      AllDays := GetAllDays(PreYear, 1, 1) - 1;
    end;

    GanZhi := GetGanZhiFromDay(Alldays + JieQis[I]);  // �õ���������յĸ�֧
    JiaZiHous[I] := JieQis[I] + (60 - GanZhi);        // �õ���������������յľ������������� 0 ��Ϊ����һ���
    JiaZiQians[I] := JiaZiHous[I] - 60;               // �õ���������ǰ�����յľ������������� 0 ��Ϊ����һ���
  end;

  JiaZiHous[0] := JiaZiHous[0] - 365;
  JiaZiQians[0] := JiaZiQians[0] - 365;
  JieQis[0] := JieQis[0] - 365;          // ������ɾ౾�����������
  if IsLeapYear(PreYear) then
  begin
    Dec(JiaZiHous[0]);
    Dec(JiaZiQians[0]);
  end;

  // JiaZiHous ������������������յľ౾�����׵��������� 0 ������Ϊ��ֵ����ʾ��ȥ��
  // JiaZiQians ������������ǰ�����յľ౾�����׵��������� 0��1 ������Ϊ��ֵ����ʾ��ȥ��
  Days := GetDayFromYearBegin(AYear, AMonth, ADay);
  for I := High(JiaZiHous) downto Low(JiaZiHous) do
  begin
    Matched := False;
    if (Days >= JieQis[I]) and (Days < JiaZiHous[I]) then
    begin
      // �����󣨺�������һ�������գ��������ڣ��ӽ���ǰ�ļ�������
      Days := Days - JiaZiQians[I];
      Matched := True;
    end
    else if Days >= JiaZiHous[I] then
    begin
      // ���ڵ��ڽ���������գ��Ӹý������������
      Days := Days - JiaZiHous[I];
      Matched := True;
    end;

    if not Matched then
      Continue;

    case I of
      0:
        begin
          // ����ǰ��һ�ף�˳��
          Result := Days mod 9;
        end;
      1:
        begin
          // ��ˮǰ���߳࣬˳��
          Result := (Days + 6) mod 9;
        end;
      2:
        begin
          // ����ǰ���ı̣�˳��
          Result := (Days + 3) mod 9;
        end;
      3:
        begin
          // ����ǰ����ϣ�����
          Result := 8 - (Days mod 9);
        end;
      4:
        begin
          // ����ǰ�����̣�����
          Result := 8 - ((Days + 6) mod 9);
        end;
      5:
        begin
          // ˪��ǰ�����ף�����
          Result := 8 - ((Days + 3) mod 9);
        end;
    end;
    Exit;
  end;
end;

// ��ȡ����ʱ��ʱ���ǣ�0-8 ��Ӧһ�׵�����
function Get9XingFromHour(AYear, AMonth, ADay, AHour: Integer): Integer;
var
  SCH, Days, DayGanZhi, DayGan, DayZhi, XiaZhi, DongZhi: Integer;
begin
  Result := -1;
  if AYear = 0 then
    Exit;

  SCH := GetShiChenFromHour(AHour);
  Days := GetDayFromYearBegin(AYear, AMonth, ADay);

  DayGanZhi := GetGanZhiFromDay(AYear, AMonth, ADay, AHour);
  ExtractGanZhi(DayGanZhi, DayGan, DayZhi);

  DongZhi := Floor(GetJieQiDayTimeFromYear(AYear, 24));
  XiaZhi := Floor(GetJieQiDayTimeFromYear(AYear, 12));

  if (Days >= XiaZhi) and (Days < DongZhi) then
  begin
    // �������Ҷ���ǰ������
    case DayZhi of
      0, 3, 6, 9:
        begin
          // ����î���յ���ʱ�Ǿ���
          Result := 8 - (SCH mod 9);
        end;
      2, 5, 8, 11:
        begin
          // �����Ⱥ��յ���ʱ������
          Result := 8 - ((SCH + 3) mod 9);
        end;
      1, 4, 7, 10:
        begin
          // �����δ�յ���ʱ������
          Result := 8 - ((SCH + 6) mod 9);
        end;
    end;
  end
  else
  begin
    // �����������ǰ��˳��
    case DayZhi of
      0, 3, 6, 9:
        begin
          // ����î���յ���ʱ��һ��
          Result := SCH mod 9;
        end;
      2, 5, 8, 11:
        begin
          // �����Ⱥ��յ���ʱ���߳�
          Result := (SCH + 6) mod 9;
        end;
      1, 4, 7, 10:
        begin
          // �����δ�յ���ʱ������
          Result := (SCH + 3) mod 9;
        end;
    end;
  end;
end;

// ��ȡ�����յ������ף�0-5 ��Ӧ��ʤ�����
function Get6YaoFromDay(AYear, AMonth, ADay: Integer): Integer;
var
  LY, LM, LD: Integer;
  Leap: Boolean;
begin
  // ũ���������
  if GetLunarFromDay(AYear, AMonth, ADay, LY, LM, LD, Leap) then
    Result := (LM + LD + 4) mod 6 // ũ�����պͳ��� 6������Ϊ 0 �Ǵ󰲣����Լ� 4
  else
    raise ECnDateTimeException.CreateFmt(SCnErrorConvertLunarDate, [AYear, AMonth, ADay]);
end;

// ���ݼ���λ���ֻ�ü���λ����
function GetJiShenFangWeiFromNumber(AFangWei: Integer): string;
begin
  Result := '';
  if (AFangWei >= 0) and (AFangWei < 8) then
    Result := SCnJiShenFangWeiArray[AFangWei];
end;

// ��ù��������յĲ���λ��0-7
function GetCaiShenFangWeiFromDay(AYear, AMonth, ADay: Integer): Integer;
var
  Gan, Zhi: Integer;
begin
  Result := -1;
  ExtractGanZhi(GetGanZhiFromDay(AYear, AMonth, ADay), Gan, Zhi);
  // �ھ������Ҷ����ǲ��񣬱�����������Ѱ���켺��������λ����������ȥ�����ɹ�ԭ�������������ǲ���λ��
  case Gan of
    0,1: Result := 1; // �����ڶ���
    2,3: Result := 5; // ����������
    4,5: Result := 0; // �켺������
    6,7: Result := 2; // ����������
    8,9: Result := 4; // �ɹ�������
  end;
end;

// ��ù��������յ�ϲ��λ��0-7
function GetXiShenFangWeiFromDay(AYear, AMonth, ADay: Integer): Integer;
var
  Gan, Zhi: Integer;
begin
  Result := -1;
  ExtractGanZhi(GetGanZhiFromDay(AYear, AMonth, ADay), Gan, Zhi);
  // �ھ����������Ҹ�Ǭ��������λϲ�񰲣����ɱ����빬�������ԭ������䡣
  // ���Զ�Ӧ��λ��Ǭ�������������������ޣ����������������㣬���ϣ��룬���ϣ��������ϣ��ң�����

  case Gan of
    0,5: Result := 1; // �׼��ڶ���
    1,6: Result := 7; // �Ҹ�������
    2,7: Result := 5; // ����������
    3,8: Result := 4; // ����������
    4,9: Result := 3; // ����ڶ���
  end;
end;

// ��ù��������յĸ���λ��0-7
function GetFuShenFangWeiFromDay(AYear, AMonth, ADay: Integer): Integer;
var
  Gan, Zhi: Integer;
begin
  Result := -1;
  ExtractGanZhi(GetGanZhiFromDay(AYear, AMonth, ADay), Gan, Zhi);
  // �����Ȼ�����׿ھ���һ�Ǵ˴��õļ׼������Ǹ��񣬱�������Ǭ���档�Ҹ���λ����ޣ�����������׷Ѱ��
  // ���ǣ����Ҷ����Ǹ��񣬱��������ǿ��ˣ��챱���ϸ�����������Ǭ�������ϡ�ɸ������á�

  case Gan of
    0,5: Result := 0; // �׼�������
    1,6: Result := 5; // �Ҹ�������
    2,7: Result := 7; // ����������
    3,8: Result := 3; // �����ڶ���
    4,9: Result := 1; // ����ڶ���
  end;
end;

// ��ù��������յĹ���λ��0-7��Ĭ��Ϊ����
function GetGuiShenFangWeiFromDay(AYear, AMonth, ADay: Integer): Integer;
begin
  Result := GetYangGuiShenFangWeiFromDay(AYear, AMonth, ADay);
end;

// ��ù��������յ�������λ��0-7
function GetYangGuiShenFangWeiFromDay(AYear, AMonth, ADay: Integer): Integer;
var
  Gan, Zhi: Integer;
begin
  Result := -1;
  ExtractGanZhi(GetGanZhiFromDay(AYear, AMonth, ADay), Gan, Zhi);

  case Gan of
    0,1: Result := 5; // ����������
    2:   Result := 6; // ��������
    3:   Result := 7; // ��������
    4,6,7: Result := 1; // ������ڶ���
    5:   Result := 0; // ��������
    8:   Result := 2; // ��������
    9:   Result := 3; // ���ڶ���
  end;
end;

// ��ù��������յ�������λ��0-7
function GetYingShenFangWeiFromDay(AYear, AMonth, ADay: Integer): Integer;
var
  Gan, Zhi: Integer;
begin
  Result := -1;
  ExtractGanZhi(GetGanZhiFromDay(AYear, AMonth, ADay), Gan, Zhi);

  case Gan of
    0:   Result := 1; // ���ڶ���
    1:   Result := 0; // ��������
    2:   Result := 7; // ��������
    3:   Result := 6; // ��������
    4,5,6: Result := 5; // �켺��������
    7:   Result := 4; // ��������
    8:   Result := 3; // ���ڶ���
    9:   Result := 2; // ��������
  end;
end;

// ��ù������������������еĵڼ��ŵĵڼ��գ�1~9,1~9 ��Ӧһ�ŵ��žţ�False Ϊ������������
function GetShu9Day(AYear, AMonth, ADay: Integer; out JiuSeq: Integer; out JiuDay: Integer): Boolean;
var
  DongZhi, Days: Integer;
begin
  Result := False;
  JiuSeq := -1;
  JiuDay := -1;

  DongZhi := Floor(GetJieQiDayTimeFromYear(AYear, 24));
  Days := GetDayFromYearBegin(AYear, AMonth, ADay);

  if (Days >= DongZhi) and (Days - DongZhi < 81) then // �ڽ���ľž���
  begin
    Result := True;
    JiuSeq := ((Days - DongZhi) div 9) + 1;
    JiuDay := ((Days - DongZhi) mod 9) + 1;
  end
  else
  begin // ����Ƿ���ǰһ�������ڵľž�
    Dec(AYear);
    if AYear = 0 then
      Dec(AYear);

    // �����һ��Ķ�����
    DongZhi := Floor(GetJieQiDayTimeFromYear(AYear, 24));

    // ��ô�������һ�����׵ĳ���
    Days := Days + 365;
    if GetIsLeapYear(AYear) then
      Inc(Days);

    if (Days >= DongZhi) and (Days - DongZhi < 81) then
    begin
      Result := True;
      JiuSeq := ((Days - DongZhi) div 9) + 1;
      JiuDay := ((Days - DongZhi) mod 9) + 1;
    end;
  end;
end;

// ��ù������������������еĵڼ����ĵڼ��գ�0~2,1~10���� 20����Ӧ������ĩ���ķ��գ�False Ϊ���ڷ�����
function Get3FuDay(AYear, AMonth, ADay: Integer; out FuSeq: Integer; out FuDay: Integer): Boolean;
var
  Days, XiaZhi, LiQiu: Integer;
  AllDays, I: Integer;
  Gan, DummyZhi: Integer;
  F1, F2, F3: Integer;
begin
  Result := False;
  FuSeq := -1;
  FuDay := -1;

  Days := GetDayFromYearBegin(AYear, AMonth, ADay);
  XiaZhi := Floor(GetJieQiDayTimeFromYear(AYear, 12)); // ���������
  LiQiu := Floor(GetJieQiDayTimeFromYear(AYear, 15)); // ���������
  AllDays := GetAllDays(AYear, 1, 1) - 1;

  for I := XiaZhi + 1 to XiaZhi + 21 do // ��֤�����������һ�����յĺ� 10 �죬�������ղ���
  begin
    if ExtractGanZhi(GetGanZhiFromDay(AllDays + I), Gan, DummyZhi) then
    begin
      if Gan = 6 then // �������һ������
      begin
        ExtractMonthDay(I, AYear, AMonth, ADay);

        F1 := I + 20; // �����գ�����������
        F2 := I + 30; // �з��գ����ĸ�����

        if (Days >= F1) and (Days < F1 + 10) then
        begin
          Result := True;
          FuSeq := 0;
          FuDay := Days - F1 + 1;
        end
        else if Days >= F2 then // �з�
        begin
          if (Days < F2 + 10) or // �з� 10 ���ڻ�����ǰ 20 ����
            ((Days >= F2 + 10) and (Days < F2 + 20) and (F2 + 10 <= LiQiu)) then
          begin
            Result := True;
            FuSeq := 1;
            FuDay := Days - F2 + 1;
          end;
        end;

        if Result then
          Exit;

        Break;
      end;
    end;
  end;

  for I := LiQiu + 1 to LiQiu + 21 do // ��֤����������һ�����յĺ� 10 �죬���ﵱ�ղ���
  begin
    if ExtractGanZhi(GetGanZhiFromDay(AllDays + I), Gan, DummyZhi) then
    begin
      if Gan = 6 then // ������һ������
      begin
        F3 := I; // ĩ��

        if (Days >= F3) and (Days < F3 + 10) then
        begin
          ExtractMonthDay(I, AYear, AMonth, ADay);
          Result := True;
          FuSeq := 2;
          FuDay := Days - F3 + 1;
        end
        else
          Result := False;

        Exit; // ������ѭ���ˣ��������ְѵڶ�����������ĩ����ʼ�Ĵ���
      end;
    end;
  end;
end;

// ���ĳ�������е���÷���ڣ�÷�꼾�ڵĿ�ʼ�գ�â�ֺ�ĵ�һ������
function GetRuMeiDay(AYear: Integer; out AMonth: Integer; out ADay: Integer): Boolean;
var
  I, MangZhong, AllDays: Integer;
  Gan, DummyZhi: Integer;
begin
  Result := False;
  MangZhong := Floor(GetJieQiDayTimeFromYear(AYear, 11)); // ���â����
  AllDays := GetAllDays(AYear, 1, 1) - 1;

  for I := MangZhong + 1 to MangZhong + 21 do
  begin
    if ExtractGanZhi(GetGanZhiFromDay(AllDays + I), Gan, DummyZhi) then
    begin
      if Gan = 2 then // â�ֺ��һ������
      begin
        ExtractMonthDay(I, AYear, AMonth, ADay);
        Result := True;
        Exit;
      end;
    end;
  end;
end;

// ���ĳ�������еĳ�÷���ڣ�÷�꼾�ڵĽ����գ�С���ĵ�һ��δ��
function GetChuMeiDay(AYear: Integer; out AMonth: Integer; out ADay: Integer): Boolean;
var
  I, XiaoShu, AllDays: Integer;
  DummyGan, Zhi: Integer;
begin
  Result := False;
  XiaoShu := Floor(GetJieQiDayTimeFromYear(AYear, 13)); // ���С����
  AllDays := GetAllDays(AYear, 1, 1) - 1;

  for I := XiaoShu + 1 to XiaoShu + 21 do
  begin
    if ExtractGanZhi(GetGanZhiFromDay(AllDays + I), DummyGan, Zhi) then
    begin
      if Zhi = 7 then // С����һ��δ��
      begin
        ExtractMonthDay(I, AYear, AMonth, ADay);
        Result := True;
        Exit;
      end;
    end;
  end;
end;

// ���ݹ��������գ����ظ����������������ָ����ݣ�Ҳ����˵�����պ��ǽ��꣬����Ϊȥ��
function GetYearSeperatedByLiChun(AYear, AMonth, ADay: Integer): Integer;
var
  Days: Extended;
begin
  Result := AYear;
  Days := GetDayFromYearBegin(AYear, AMonth, ADay);

  // �籾����������ǰ����������ǰһ��
  if Days < GetJieQiDayTimeFromYear(AYear, 3) then
  begin
    // �����Ϊǰһ��
    Dec(Result);
    if Result = 0 then // û�й�Ԫ 0 ��
      Dec(Result);
  end;
end;

// ��ֲ���й������࣬�ƺ��ǻ�ȡ�ù�����֮ǰ��������������� AYear ���� SCnLeapNumber �Ķ�����˵
// �������й�Ԫ 0 ����������ֵ��Ҳ����˵��Ԫǰ 850 ��Ӧ�ô� -849�����㵽�±� -1��
function GetLeapNum(AYear: Integer): Integer;
begin
  // ǰ�� 850 ���ǹ�Ԫ��� -850 �� -1 �����������±�� 0 ��ʼ��[0..849] �� 850 ���ǹ�Ԫǰ��
  // �����Ԫǰĳ�꣬��ֵ��������Ʃ�� -850 �꣬Ҫ��Ӧ���±� 0�����Եü� 850
  if AYear < 0 then
    Result := SCnLeapNumber[AYear + 848]      // -1 ��ʾ�Ĺ�Ԫǰ 2 �꣬Ҫ 847
  else
    Result := SCnLeapNumber[AYear - 1 + 849]; // ��ԪԪ�� 1���±����õ� 849��0 ��ʾ�Ĺ�Ԫǰ 1 ��Ҫ 848
end;

// ��ֲ���й������࣬����� AYear �ǹ�����ݣ�û�й�Ԫ 0 �꣬Ҳ����Ԫǰ 850 ��Ҫ�� -850
// �����ڲ������ݴ������ 0 �ҹ�Ԫ 0 ������£��᷵�� 0 ��ʾû������
function GetLeapMonth(AYear: Integer): Integer;
var
  C: Char;
begin
  C := SCnLeapMonth[AYear + 850]; // �ַ����±��� 1 ��ʼ��

{$IFDEF UNICODE}
  if CharInSet(C, ['0'..'9']) then
    Result := StrToInt(C)
  else if CharInSet(C , ['a'..'c']) then
    Result := 10 + Ord(C) - Ord('a')
  else
    Result := -1;
{$ELSE}
  if C in ['0'..'9'] then
    Result := StrToInt(C)
  else if C in ['a'..'c'] then
    Result := 10 + Ord(C) - Ord('a')
  else
    Result := -1;
{$ENDIF}
end;

// ���һ�����������С������
function GetTail(X: Real): Real;
begin
  if X > 0 then
    Result := X - Trunc(X)
  else
    Result := X + Trunc(X);
end;

// ĳ�Ƕȼ��㺯������ֲ���й�������
function GetAng(X, T, C1, T0, T2, T3: Real): Real;
begin
  Result := GetTail(C1 * X) * 2 * Pi + T0 - T2 * T * T - T3 * T * T * T;
end;

// ���ĳ���������յ�ũ�������͸��������Լ�����ʳ���ͺ�ʱ�̣��������ƺ�Ҫ�� 0 ������
// Ҳ����Ԫǰ 1 ��Ҫ�� 0����Ԫǰ 850 ��Ҫ�� -849
function GetLunarMoon(AYear, AMonth, ADay: Integer; out EclipseType: TCnEclipseType;
  out MoonPhase: TCnMoonPhase; out TheTime: Double): Real;
var
  K, K1: Real;
  T, Rpi, Zone, F0, Fc, J0, Aa0, Ab0, Ac0, ShuoTime, WangTime: Real;
  Aa, Ab, Ac, F1, J: Real;
  I, Ms, LunDay, LunDay0, WangDay: Integer;
  S, R, P, Q: Real;
  StdDays: Integer;
begin
  T := (AYear - 1899.5) / 100;
  Ms := Floor((AYear - 1900) * 12.3685);
  Rpi := 180 / Pi;
  Zone := 8;
  F0 := GetAng(Ms, T, 0, 0.75933, 2.172e-4, 1.55e-7)
    + 0.53058868 * Ms - 8.37e-4 * T + Zone / 24 + 0.5;
  Fc := 0.1734 - 3.93e-4 * T;
  J0 := 693595 + 29 * Ms;
  Aa0 := GetAng(Ms, T, 0.08084821133, 359.2242/Rpi, 0.0000333/Rpi, 0.00000347/Rpi);
  Ab0 := GetAng(Ms, T, 7.171366127999999e-2, 306.0253/Rpi, -0.0107306/Rpi, -0.00001236/Rpi);
  Ac0 := GetAng(Ms, T, 0.08519585128, 21.2964/Rpi, 0.0016528/Rpi, 0.00000239/Rpi);

  EclipseType := etNone;
  LunDay := -1;

  ShuoTime := 0;
  WangDay := 0;
  WangTime := 0;

  K1 := -1; K := -1;
  StdDays := GetEquStandardDays(AYear, AMonth, ADay);
  while K <= 13 do
  begin
    Aa := Aa0 + 0.507984293 * K;
    Ab := Ab0 + 6.73377553 * K;
    Ac := Ac0 + 6.818486628 * K;
    F1 := F0 + 1.53058868 * K + Fc * Sin(Aa) - 0.4068 * Sin(Ab)
      + 0.0021 * Sin(2 * Aa) + 0.0161 * Sin(2 * Ab) + 0.0104 * Sin(2 * Ac)
      - 0.0074 * Sin(Aa - Ab) - 0.0051 * Sin(Aa + Ab);

    J := J0 + 28 * K + F1;

    LunDay0 := StdDays - Floor(J);
    if (K = Floor(K)) and (LunDay0 >= 0) and (LunDay0 <= 29) then
    begin
      K1 := K;
      ShuoTime := GetTail(J);
      LunDay := LunDay0 + 1;
    end;

    if (K = K1 + 0.5) then
    begin
      WangTime := GetTail(J);
      WangDay := Floor(J) - (StdDays - LunDay + 1) + 1;
    end;

    if((LunDay = 1) and (K = K1)) or
      ((LunDay = WangDay) and (K = K1 + 0.5)) then
    begin
      if Abs(Sin(Ac))<= 0.36 then
      begin
        S := 5.19595 - 0.0048 * Cos(Aa) + 0.002 * Cos(2 * Aa) - 0.3283 * Cos(Ab)
          - 0.006 * Cos(Aa + Ab) + 0.0041 * Cos(Aa - Ab);
        R := 0.207 * Sin(Aa) + 0.0024 * Sin(2 * Aa) - 0.039 * Sin(Ab)
          + 0.0115 * Sin(2 * Ab) - 0.0073 * Sin(Aa + Ab) - 0.0067 * Sin(Aa - Ab)
          + 0.0117 * Sin(2 * Ac);
        P := Abs(S * Sin(Ac) + R * Cos(Ac));
        Q := 0.0059 + 0.0046 * Cos(Ac) - 0.0182 * Cos(Ab) + 0.0004 * Cos(2 * Ab)
          - 0.0005 * Cos(Aa + Ab);

        if P - Q <= 1.5572 then
        begin
          EclipseType := etSolar; // ��ʳ
          if K <> Floor(K) then
          begin
            if P + Q >= 1.0129 then
              EclipseType := etMoonHalf   // ��ƫʳ
            else
              EclipseType := etMoonFull;  //��ȫʳ
          end;
        end;
      end;
    end;

    K := K + 0.5;
  end;

  // ��ʷ�ϵĹ۲�ƫ��µĵ���ũ�����׵ĵ���ƫ��������������������
  for I := Low(CN_LUNAR_SINGLE_MONTH_FIX) to High(CN_LUNAR_SINGLE_MONTH_FIX) do
  begin
    if (AYear = CN_LUNAR_SINGLE_MONTH_FIX[I].Year)
      and (((AMonth = CN_LUNAR_SINGLE_MONTH_FIX[I].Month) and (ADay >= CN_LUNAR_SINGLE_MONTH_FIX[I].StartDay))
      or ((AMonth = CN_LUNAR_SINGLE_MONTH_FIX[I].Month + 1) and (ADay <= CN_LUNAR_SINGLE_MONTH_FIX[I].EndDay))) then
    begin
      if CN_LUNAR_SINGLE_MONTH_FIX[I].IncOne then
      begin
        Inc(LunDay);
        if LunDay > 30 then
          LunDay := Lunday - 30;
      end
      else
      begin
        Dec(LunDay);
        if LunDay < 1 then
          LunDay := LunDay + 30;
      end;
    end;
  end;

  // 245.12.6 �� 246.1.4 �������ƫ��¸��²�һ��Ҫ����
  if ((AYear = 245) and ((AMonth = 12) and (ADay >= 6)))
    or ((AYear = 246) and ((AMonth = 1) and (ADay <= 4))) then
  begin
    Inc(LunDay);
    if LunDay > 30 then
      LunDay := Lunday - 30;
  end;

  Result := LunDay;

  if LunDay = 1 then // ˷��
  begin
    MoonPhase := mpShuo;
    TheTime := ShuoTime;
  end
  else if LunDay = WangDay then
  begin
    MoonPhase := mpWang;
    TheTime := WangTime;
  end
  else
  begin
    MoonPhase := mpNone;
    TheTime := -1;
  end;
end;

// ���ĳũ��������£����� 1~12 ��Ӧһ�µ�ʮ���£����� 0 ��ʾ������
function GetLunarLeapMonth(AYear: Integer): Integer;
begin
  Result := GetLeapMonth(AYear);
  if Result < 0 then
    Result := 0;
end;

// ���ĳ���������յ�ũ��������������������ø�ֵ��ʾ��
// ���й�������ƺ�Ҫ�� 0 ������Ҳ����Ԫǰ 1 ��Ҫ�� 0����Ԫǰ 850 ��Ҫ�� -849
function GetLunarMonth(AYear, AMonth, ADay: Integer): Real;
var
  LunDay: Real;
  aEclipsType: TCnEclipseType;
  aMoonPhase: TCnMoonPhase;
  aTime: Double;
  LeapMons, NMonth, LMY, LMY1: Integer;

  // С����������
  function GetRemain(X, W: Real): Real;
  begin
    Result := GetTail(X/W) * W;
  end;

begin
  LunDay := GetLunarMoon(AYear, AMonth, ADay, aEclipsType, aMoonPhase, aTime);
  if aTime <> -1 then
    LunDay := LunDay + aTime;
  LunDay := Floor(LunDay - Floor(LunDay / 100) * 100);

  LeapMons := GetLeapNum(AYear);
  NMonth := Round((GetEquStandardDays(AYear, AMonth, ADay)
    - GetEquStandardDays(-849, 1, 21) - LunDay)/ 29.530588) - LeapMons;
  // ������ -849 ����Ϊ�� GetEquStandardDays Ҫ���������ݵ������£���Ԫǰ 850 ���� -849

  //��ʷ�ϵ��޸��½�
  if (AYear < 240) or ((AYear = 240) and (AMonth = 1) and (ADay < 12)) then
    Inc(NMonth);  // ��Ԫ 239 �� 12 �� 13 ��ũ��ʮ���´�240 �� 1 �� 12 ������ʮ����С���ֲ�������

  if AYear <= 237 then Dec(NMonth);

  if (AYear < 24) and not ((AYear = 23) and (AMonth = 12) and (ADay = 31)) then  // 23 �� 12 �� 31 ��Ҳ���ܼ� 1
    Inc(NMonth);  // ��Ԫ 23 �� 12 �� 2 ��ʮ����С��12 �� 31 ������ʮ���´�Ҳ��������

  if AYear < 9 then
    Dec(NMonth);
  if AYear <= -255 then
    Inc(NMonth);
  if AYear <= -256 then
    Inc(NMonth, 2);
  if AYear <= -722 then
    Inc(NMonth);

  LMY := GetLeapMonth(AYear);
  LMY1 := GetLeapMonth(AYear - 1);

  Result := Round(GetRemain(NMonth - 3, 12) + 1); // Result �õ������£���������������С���ʲ��� AYear

  if ((Result = LMY1) and (AMonth = 1) and (ADay < LunDay))
    or ((Result = LMY1) and (LMY1 = 12) and (AMonth in [1, 2])) then
    // ��һ��������С�ĵز���ȥ�ģ��������ȥ���� 12 �£����깫���·� 1 �� 2�����������ũ������ 12�����ũ���������£������������ж� ADay �� LunDay �Ĺ�ϵ
  begin
    Result := -Result;    // ��� AYear - 1 ��ĩ�������Ҹ��½ӵ��� AYear ��,�� AYear �����Ҳ������
  end
  else if Result = LMY then
  begin
    // ����õ����·����뵱�����������ͬ������ 1612 �� 1 �� 31 �š�
    // ����������õ��� 11 �£����� 1612 ������и��� 11 �£��������ܻ���
    // ����������� 1 ������ 1���������ͬһ��
    if (Result <> 1) and ((AMonth in [1, 2]) and (LMY <> 12)) then
    begin
      // �����жϣ���������·���������ҽ������²��� 12 �£��ʹ����˵�������²���һ����ģ�
      // ���Բ������£�����Ϊ��ͨ�¡�������������ܲ���̫׼ȷ

      // ���� 1984 ������ 10 �£��� 1984.1.1 ��ũ����Ϊ 10��
      // �����Ǵ� 1983 �������ӹ����ģ����Բ��� 1984 ����� 10 ��

      Result := Result + 1;
    end
    else if ((AMonth in [1, 2]) or ((AMonth = 3) and (AYear <= 436))) and (Result = 12) then
    begin
      // ��Ҫ���������������Ԫ 1574 �� 1��2 ��תũ���õ� 12 �£��� 1574 ���� 12 �£��������Ҳ�������£�Ҫ����Ϊ��ͨ��
      // ��Ԫ 436 �� 3 �� 1 ��תũ���õ� 12 �£��� 436 ���� 12 �£��������Ҳ�������£�ͬ��Ҫ����Ϊ��ͨ�£��� 3 �����������ڹ�Ԫ 436 ֮ǰ
      Result := 1;  // 12 + 1 - 12 = 1
    end
    else
    begin
      Result := -Result; // �ø���ʾ����
    end;
  end
  else
  begin
    // �����һ�Ĳ���
    if ((Result < LMY) or (AMonth < Result)) and (LMY > 0) then
    begin
      // ��� AYear �������µ�����δ��������ǰ���۳��˱�������£�����Ӧ������
      // ����� AYear �����ˣ�ʵ��ũ������ǰһ����繫Ԫ 1575 01 01������ͻ�©����һ�������ٲ���
      Result := Result + 1;
    end
    else if (Result >= 10) and (AMonth in [1, 2]) and (LMY1 > 0 ) and (Result < LMY1) then
    begin
      // ������Ϊ������ 1 �� 2 ��ũ���� 10 ���Ժ󣬱ض������˿��꣬��˵���ǰһ���������
      Result := Result + 1;
    end;

    Result := Round(GetRemain(Result - 1, 12) + 1);
  end;

{
   ��Ԫ 239 �� 12 �� 13 ��ʮ���´�󣬹�Ԫ 240 �� 1 �� 12 ������ʮ����С�������������£�
   ��Ϊ����ʾ���֣�240 �� 1 �� 12 �ռ��Ժ��ʮ���·��� IsLeapMonth Ϊ True��
   ��Ԫ 23 �� 12 �� 2 ��ʮ����С��12 �� 31 ������ʮ���´�Ҳ�������£�
   ͬ��Ϊ����ʾ���֣�12 �� 31 �ռ��Ժ��ʮ���·��� IsLeapMonth Ϊ True��
}

   if Result = 12 then
   begin
     if AYear = 240 then
     begin
       if ((AMonth = 1) and (ADay >= 12)) or ((AMonth = 2) and (ADay <= 9)) then
         Result := -Result;
     end
     else if (AYear = 23) and (AMonth = 12) and (ADay = 31) then
       Result := -Result
     else if (AYear = 24) and (AMonth = 1) and (ADay <= 29) then
       Result := -Result;
   end;
end;

// ���ĳ���������յ�ũ�������պ��Ƿ����µ���Ϣ
function GetLunarFromDay(AYear, AMonth, ADay: Integer;
  out LunarYear, LunarMonth, LunarDay: Integer; out IsLeapMonth: Boolean): Boolean;
var
  aEclipsType: TCnEclipseType;
  aMoonPhase: TCnMoonPhase;
  aTime: Double;
begin
  Result := False;

  // 0 ��������Ԫ�꣬ת�� 0 ������Ԫ�꣬Ҳ����Ԫǰ 850 ���� -849
  NonZeroYearToZeroYear(AYear);

  if (AYear >= -849) and (AYear <= 2800) then
  begin
    LunarDay := Floor(GetLunarMoon(AYear, AMonth, ADay, aEclipsType, aMoonPhase, aTime));
    LunarMonth := Floor(GetLunarMonth(AYear, AMonth, ADay));
    IsLeapMonth := LunarMonth < 0;
    if IsLeapMonth then
      LunarMonth := - LunarMonth;
    LunarYear := AYear;

    // ũ�����°��꣬�������ϰ��꣬��ũ��ӦΪ��һ��
    if (LunarMonth > 6) and (AMonth < 6) then
      Dec(LunarYear);

    // ��Ԫ������������������ 12 �£�ũ���� 1 �£�������һ��
    if (LunarMonth = 1) and (AMonth = 12) then
      Inc(LunarYear);

    ZeroYearToNonZeroYear(LunarYear); // ������ũ����ת�ɷ�������ũ���꣬û��ũ�� 0 ��
    Result := True;
  end;
end;

// ���ĳ���������յ�ũ�����պ��Ƿ����µ���Ϣ
function GetLunarMonthDayFromDay(AYear, AMonth, ADay: Integer;
  out LunarMonth, LunarDay: Integer; out IsLeapMonth: Boolean): Boolean;
var
  aEclipsType: TCnEclipseType;
  aMoonPhase: TCnMoonPhase;
  aTime: Double;
begin
  Result := False;

  // 0 ��������Ԫ�꣬ת�� 0 ������Ԫ�꣬Ҳ����Ԫǰ 850 ���� -849
  NonZeroYearToZeroYear(AYear);

  if (AYear >= -849) and (AYear <= 2800) then
  begin
    LunarDay := Floor(GetLunarMoon(AYear, AMonth, ADay, aEclipsType, aMoonPhase, aTime));
    LunarMonth := Floor(GetLunarMonth(AYear, AMonth, ADay));
    IsLeapMonth := LunarMonth < 0;
    if IsLeapMonth then
      LunarMonth := - LunarMonth;
    Result := True;
  end;
end;

// ���ĳũ�������գ����Ƿ����£��Ĺ���������
// �ú������÷�����ַ�����
function GetDayFromLunar(ALunarYear, ALunarMonth, ALunarDay: Integer; IsLeapMonth:
  Boolean; out AYear, AMonth, ADay: Integer): Boolean;
type
  TLunarSearchDirection = (lsdInvalid, lsdUp, lsdDown);
var
  StartYear, StartMonth, StartDay: Integer;
  EndYear, EndMonth, EndDay: Integer;
  StartDays, EndDays, InterDays: Integer;
  TempYear, TempMonth, TempDay: Integer;
  TempLunarYear, TempLunarMonth, TempLunarDay, OldTempLunarMonth: Integer;
  TempIsLeap, Only2: Boolean;
  Lsd: TLunarSearchDirection;
  Count: Integer;
begin
  Result := False;
  if IsLeapMonth and (GetLunarLeapMonth(ALunarYear) <> ALunarMonth)
    and (GetLunarAdditionalLeapMonth(ALunarYear) <> ALunarMonth) then
    Exit; // �����޴����»�����������˳�


  // ��ʼ��ΧΪ��������һ��һ�յ�����ʮ������ʮһ�գ���������ǰ������ʷ�����³�һ
  // û���䵽��������ǰȥ�������������������ɿ����ʵ�����������Χ�������
  // ��һ������һ��һ�յ�����ʮ������ʮһ�գ����ֿ������������������Χ�жϵĴ�ֻ�ֿܷ�����
  // ȷ��������Χ���ֻ������

  if (ALunarYear < 20) and (ALunarMonth in [1, 2]) then
  begin
    // ��Ԫ��ʮ��ķ�Χ�ڣ����³�һ�����䵽������ǰ�����������ݸĳ�ǰһ������������
    StartYear := ALunarYear - 1;
    if StartYear = 0 then
      Dec(StartYear);

    EndYear := ALunarYear;
  end
  else
  begin
    StartYear := ALunarYear;

    EndYear := ALunarYear + 1;
    if EndYear = 0 then // û�й�Ԫ 0 ��ͬ��û��ũ�� 0 ��
      EndYear := 1;
  end;

  StartMonth := 1;
  StartDay := 1;

  EndMonth := 12;
  EndDay := 31;

  StartDays := Trunc(GetJulianDate(StartYear, StartMonth, StartDay));
  EndDays := Trunc(GetJulianDate(EndYear, EndMonth, EndDay));

  Only2 := False;
  Lsd := lsdInvalid;
  TempYear := StartYear;
  TempLunarYear := StartYear;
  OldTempLunarMonth := 0;

  Count := 0;
  while StartDays < EndDays do
  begin
    Inc(Count);
    if Count > 100 then // ����������ѭ��
      Exit;

    InterDays := (StartDays + EndDays) div 2;
    if Only2 then
      Inc(InterDays);

    if EndDays - StartDays = 1 then
      Only2 := True;

    GetDayFromJulianDate(InterDays, TempYear, TempMonth, TempDay);
    GetLunarMonthDayFromDay(TempYear, TempMonth, TempDay, TempLunarMonth,
      TempLunarDay, TempIsLeap);
    // ��ת������ֱ�ӻ�ȡ��ݣ�����������ж�����ȡ���

    if (Lsd = lsdInvalid) and (Count = 1) then
    begin
      // ��һ�ν���ʱ������ҲҪ����ũ��ת����ݣ������������׵��£���ũ��ת������������£�˵�����ڵڶ���
      if (TempMonth > 10) and (TempLunarMonth <= 2) then
      begin
        Inc(TempLunarYear);
        if TempLunarYear = 0 then
          TempLunarYear := 1;
      end
      else if (TempLunarYear = StartYear) and (TempMonth = TempLunarMonth) and (TempMonth = 1) then
      begin
        // �ڶ����������һ�ε��м���Ȼ�ǵ�һ�����ǰ���������ũ���·ݶ�Ϊ 1 ���ʾ�����ڵڶ����
        Inc(TempLunarYear);
        if TempLunarYear = 0 then
          TempLunarYear := 1;
      end;
    end;

    case Lsd of
      lsdUp:
        begin
          // ��δ������ʱ���ũ�����ɴ��С�ˣ�˵�������꣬��ݵü�һ
          if TempLunarMonth < OldTempLunarMonth then
          begin
            Inc(TempLunarYear);
            if TempLunarYear = 0 then
              TempLunarYear := 1;
          end;
        end;
      lsdDown:
        begin
          // ����ȥ����ʱ���ũ������С����ˣ�˵�������꣬��ݵü�һ
          if TempLunarMonth > OldTempLunarMonth then
          begin
            Dec(TempLunarYear);
            if TempLunarYear = 0 then
              TempLunarYear := -1;
          end;
        end;
    end;

    case Compare2LunarDay(TempLunarYear, TempLunarMonth, TempLunarDay, TempIsLeap,
      ALunarYear, ALunarMonth, ALunarDay, IsLeapMonth) of
      -1:
        begin
          StartDays := InterDays;
          Lsd := lsdUp; // ��δ������
        end;
      0:
        begin
          AYear := TempYear;
          AMonth := TempMonth;
          ADay := TempDay;
          Result := True;
          Exit;
        end;
      1:
        begin
          EndDays := InterDays;
          Lsd := lsdDown; // ����ȥ����
        end;
    end;
    OldTempLunarMonth := TempLunarMonth;
  end;
end;

end.
