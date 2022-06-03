object MainForm: TMainForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'AWS Transcribe Sample'
  ClientHeight = 494
  ClientWidth = 514
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  TextHeight = 15
  object PageControl: TPageControl
    AlignWithMargins = True
    Left = 4
    Top = 4
    Width = 506
    Height = 486
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    ActivePage = FiltersTab
    Align = alClient
    TabHeight = 28
    TabOrder = 0
    OnChange = PageControlChange
    object JobsTab: TTabSheet
      Caption = 'Jobs'
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 498
        Height = 40
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object Label2: TLabel
          AlignWithMargins = True
          Left = 4
          Top = 4
          Width = 89
          Height = 32
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Align = alLeft
          Caption = 'Trancription Jobs'
          Layout = tlCenter
          ExplicitHeight = 15
        end
        object ListJobsButton: TSpeedButton
          AlignWithMargins = True
          Left = 101
          Top = 6
          Width = 75
          Height = 28
          Margins.Left = 4
          Margins.Top = 6
          Margins.Right = 4
          Margins.Bottom = 6
          Align = alLeft
          Caption = 'Refresh'
          OnClick = ListJobsButtonClick
        end
        object DeleteJobButton: TSpeedButton
          AlignWithMargins = True
          Left = 419
          Top = 6
          Width = 75
          Height = 28
          Margins.Left = 4
          Margins.Top = 6
          Margins.Right = 4
          Margins.Bottom = 6
          Align = alRight
          Caption = 'Delete'
          Enabled = False
          OnClick = DeleteJobButtonClick
          ExplicitLeft = 354
        end
        object ViewJobButton: TSpeedButton
          AlignWithMargins = True
          Left = 336
          Top = 6
          Width = 75
          Height = 28
          Margins.Left = 4
          Margins.Top = 6
          Margins.Right = 4
          Margins.Bottom = 6
          Align = alRight
          Caption = 'View'
          Enabled = False
          OnClick = ViewJobButtonClick
          ExplicitLeft = 331
        end
        object CreateJobButton: TSpeedButton
          AlignWithMargins = True
          Left = 253
          Top = 6
          Width = 75
          Height = 28
          Margins.Left = 4
          Margins.Top = 6
          Margins.Right = 4
          Margins.Bottom = 6
          Align = alRight
          Caption = 'New'
          OnClick = CreateJobButtonClick
          ExplicitLeft = 314
        end
      end
      object JobsView: TListView
        AlignWithMargins = True
        Left = 4
        Top = 44
        Width = 490
        Height = 400
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alClient
        Columns = <
          item
            AutoSize = True
            Caption = 'Name'
          end
          item
            Alignment = taCenter
            Caption = 'Lang'
            MaxWidth = 60
            MinWidth = 60
            Width = 60
          end
          item
            Alignment = taCenter
            Caption = 'Created'
            MaxWidth = 130
            MinWidth = 130
            Width = 130
          end
          item
            Alignment = taCenter
            Caption = 'Status'
            MaxWidth = 90
            MinWidth = 90
            Width = 90
          end>
        ColumnClick = False
        DoubleBuffered = True
        HideSelection = False
        OwnerData = True
        ReadOnly = True
        RowSelect = True
        ParentDoubleBuffered = False
        TabOrder = 1
        ViewStyle = vsReport
        OnChange = JobsViewChange
        OnCustomDrawSubItem = JobsViewCustomDrawSubItem
        OnData = JobsViewData
      end
    end
    object FiltersTab: TTabSheet
      Caption = 'Filters'
      ImageIndex = 1
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 498
        Height = 40
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object Label3: TLabel
          AlignWithMargins = True
          Left = 4
          Top = 4
          Width = 92
          Height = 32
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Align = alLeft
          Caption = 'Vocabulary Filters'
          Layout = tlCenter
          ExplicitHeight = 15
        end
        object ListFiltersButton: TSpeedButton
          AlignWithMargins = True
          Left = 104
          Top = 6
          Width = 75
          Height = 28
          Margins.Left = 4
          Margins.Top = 6
          Margins.Right = 4
          Margins.Bottom = 6
          Align = alLeft
          Caption = 'Refresh'
          OnClick = ListFiltersButtonClick
          ExplicitLeft = 101
        end
        object DeleteFilterButton: TSpeedButton
          AlignWithMargins = True
          Left = 419
          Top = 6
          Width = 75
          Height = 28
          Margins.Left = 4
          Margins.Top = 6
          Margins.Right = 4
          Margins.Bottom = 6
          Align = alRight
          Caption = 'Delete'
          Enabled = False
          OnClick = DeleteFilterButtonClick
          ExplicitLeft = 354
        end
        object ViewFilterButton: TSpeedButton
          AlignWithMargins = True
          Left = 336
          Top = 6
          Width = 75
          Height = 28
          Margins.Left = 4
          Margins.Top = 6
          Margins.Right = 4
          Margins.Bottom = 6
          Align = alRight
          Caption = 'View'
          Enabled = False
          OnClick = ViewFilterButtonClick
          ExplicitLeft = 331
        end
        object CreateFilterButton: TSpeedButton
          AlignWithMargins = True
          Left = 253
          Top = 6
          Width = 75
          Height = 28
          Margins.Left = 4
          Margins.Top = 6
          Margins.Right = 4
          Margins.Bottom = 6
          Align = alRight
          Caption = 'New'
          OnClick = CreateFilterButtonClick
          ExplicitLeft = 314
        end
      end
      object FiltersView: TListView
        AlignWithMargins = True
        Left = 4
        Top = 44
        Width = 490
        Height = 400
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alClient
        Columns = <
          item
            AutoSize = True
            Caption = 'Name'
          end
          item
            Alignment = taCenter
            Caption = 'Lang'
            MaxWidth = 60
            MinWidth = 60
            Width = 60
          end
          item
            Alignment = taCenter
            Caption = 'Last Modified'
            MaxWidth = 130
            MinWidth = 130
            Width = 130
          end>
        ColumnClick = False
        DoubleBuffered = True
        HideSelection = False
        OwnerData = True
        ReadOnly = True
        RowSelect = True
        ParentDoubleBuffered = False
        TabOrder = 1
        ViewStyle = vsReport
        OnChange = FiltersViewChange
        OnData = FiltersViewData
      end
    end
  end
end
