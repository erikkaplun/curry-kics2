import qualified System.Time as T

instance ConvertCurryHaskell C_ClockTime T.ClockTime where
  fromCurry (C_CTime i) = T.TOD (fromCurry i) 0
  toCurry   (T.TOD i _) = C_CTime (toCurry i)

instance ConvertCurryHaskell C_CalendarTime T.CalendarTime where
  fromCurry (C_CalendarTime y m d h min s tz ) = 
           T.CalendarTime (fromCurry y) 
                          (toEnum (fromCurry m - 1))
                          (fromCurry d)
                          (fromCurry h)
                          (fromCurry min)
                          (fromCurry s)
                          0 undefined undefined undefined
                          (fromCurry tz)
                          undefined
           
  toCurry (T.CalendarTime y m d h min s _ _ _ _ tz _) = 
          C_CalendarTime (toCurry y) 
                         (toCurry (fromEnum m + 1)) 
                         (toCurry d)
                         (toCurry h)
                         (toCurry min)
                         (toCurry s)
                         (toCurry tz)

external_d_C_getClockTime :: C_IO C_ClockTime
external_d_C_getClockTime =
  fromIO (T.getClockTime >>= return . toCurry)

external_d_C_prim_toCalendarTime :: C_ClockTime -> C_IO C_CalendarTime
external_d_C_prim_toCalendarTime ct =
  fromIO (T.toCalendarTime (fromCurry ct) >>= return . toCurry)

external_d_C_prim_toUTCTime :: C_ClockTime -> C_CalendarTime
external_d_C_prim_toUTCTime ct = toCurry (T.toUTCTime (fromCurry ct))

external_d_C_prim_toClockTime :: C_CalendarTime -> C_ClockTime
external_d_C_prim_toClockTime ct =
  toCurry (T.toClockTime (fromCurry ct))
