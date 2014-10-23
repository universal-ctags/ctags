      MODULE inm_df
      IMPLICIT none
      SAVE
      TYPE df_type
      REAL(8), POINTER :: &
       df_mb_time(:),              df_wb_time(:)
      REAL(4), POINTER :: &
       df_mb_data(:,:),  df_wb_data(:,:)
      END TYPE
      END MODULE inm_df
