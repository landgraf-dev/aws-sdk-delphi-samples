object Form8: TForm8
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'AWS Rekognition'
  ClientHeight = 386
  ClientWidth = 158
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  DesignSize = (
    158
    386)
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 97
    Height = 25
    Caption = 'Load image...'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 39
    Width = 97
    Height = 25
    Caption = 'Detect text'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 183
    Width = 97
    Height = 25
    Caption = 'Detect celebrities'
    TabOrder = 2
    OnClick = Button3Click
  end
  object lbText: TListBox
    Left = 8
    Top = 72
    Width = 142
    Height = 105
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 3
    ExplicitWidth = 212
  end
  object lbCelebrities: TListBox
    Left = 8
    Top = 214
    Width = 142
    Height = 163
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 4
    ExplicitWidth = 212
    ExplicitHeight = 105
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Left = 73
    Top = 104
  end
end
