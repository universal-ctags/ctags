! Test of parsing type with invalid name
      Module TR_DetectionRecording
      Implicit None
      Save
      Integer :: DR_Lun
      Character*1024  DR_Filename
      Type Detection_Record
         Integer(2) :: Activity
         Integer(2) :: Type           ! "Type" not valid entity name
         Integer(2) :: Face
      End Type Detection_Record
      End Module TR_DetectionRecording
