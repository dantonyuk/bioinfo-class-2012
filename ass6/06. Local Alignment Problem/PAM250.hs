module PAM250 where

import Prelude hiding (lookup)
import Data.Map

scores :: Map (Char,Char) Int
scores = fromList [
    (('A','A'),2),(('A','C'),-2),(('A','D'),0),(('A','E'),0),(('A','F'),-3),(('A','G'),1),(('A','H'),-1),(('A','I'),-1),(('A','K'),-1),(('A','L'),-2),(('A','M'),-1),(('A','N'),0),(('A','P'),1),(('A','Q'),0),(('A','R'),-2),(('A','S'),1),(('A','T'),1),(('A','V'),0),(('A','W'),-6),(('A','Y'),-3),
    (('C','A'),-2),(('C','C'),12),(('C','D'),-5),(('C','E'),-5),(('C','F'),-4),(('C','G'),-3),(('C','H'),-3),(('C','I'),-2),(('C','K'),-5),(('C','L'),-6),(('C','M'),-5),(('C','N'),-4),(('C','P'),-3),(('C','Q'),-5),(('C','R'),-4),(('C','S'),0),(('C','T'),-2),(('C','V'),-2),(('C','W'),-8),(('C','Y'),0),
    (('D','A'),0),(('D','C'),-5),(('D','D'),4),(('D','E'),3),(('D','F'),-6),(('D','G'),1),(('D','H'),1),(('D','I'),-2),(('D','K'),0),(('D','L'),-4),(('D','M'),-3),(('D','N'),2),(('D','P'),-1),(('D','Q'),2),(('D','R'),-1),(('D','S'),0),(('D','T'),0),(('D','V'),-2),(('D','W'),-7),(('D','Y'),-4),
    (('E','A'),0),(('E','C'),-5),(('E','D'),3),(('E','E'),4),(('E','F'),-5),(('E','G'),0),(('E','H'),1),(('E','I'),-2),(('E','K'),0),(('E','L'),-3),(('E','M'),-2),(('E','N'),1),(('E','P'),-1),(('E','Q'),2),(('E','R'),-1),(('E','S'),0),(('E','T'),0),(('E','V'),-2),(('E','W'),-7),(('E','Y'),-4),
    (('F','A'),-3),(('F','C'),-4),(('F','D'),-6),(('F','E'),-5),(('F','F'),9),(('F','G'),-5),(('F','H'),-2),(('F','I'),1),(('F','K'),-5),(('F','L'),2),(('F','M'),0),(('F','N'),-3),(('F','P'),-5),(('F','Q'),-5),(('F','R'),-4),(('F','S'),-3),(('F','T'),-3),(('F','V'),-1),(('F','W'),0),(('F','Y'),7),
    (('G','A'),1),(('G','C'),-3),(('G','D'),1),(('G','E'),0),(('G','F'),-5),(('G','G'),5),(('G','H'),-2),(('G','I'),-3),(('G','K'),-2),(('G','L'),-4),(('G','M'),-3),(('G','N'),0),(('G','P'),0),(('G','Q'),-1),(('G','R'),-3),(('G','S'),1),(('G','T'),0),(('G','V'),-1),(('G','W'),-7),(('G','Y'),-5),
    (('H','A'),-1),(('H','C'),-3),(('H','D'),1),(('H','E'),1),(('H','F'),-2),(('H','G'),-2),(('H','H'),6),(('H','I'),-2),(('H','K'),0),(('H','L'),-2),(('H','M'),-2),(('H','N'),2),(('H','P'),0),(('H','Q'),3),(('H','R'),2),(('H','S'),-1),(('H','T'),-1),(('H','V'),-2),(('H','W'),-3),(('H','Y'),0),
    (('I','A'),-1),(('I','C'),-2),(('I','D'),-2),(('I','E'),-2),(('I','F'),1),(('I','G'),-3),(('I','H'),-2),(('I','I'),5),(('I','K'),-2),(('I','L'),2),(('I','M'),2),(('I','N'),-2),(('I','P'),-2),(('I','Q'),-2),(('I','R'),-2),(('I','S'),-1),(('I','T'),0),(('I','V'),4),(('I','W'),-5),(('I','Y'),-1),
    (('K','A'),-1),(('K','C'),-5),(('K','D'),0),(('K','E'),0),(('K','F'),-5),(('K','G'),-2),(('K','H'),0),(('K','I'),-2),(('K','K'),5),(('K','L'),-3),(('K','M'),0),(('K','N'),1),(('K','P'),-1),(('K','Q'),1),(('K','R'),3),(('K','S'),0),(('K','T'),0),(('K','V'),-2),(('K','W'),-3),(('K','Y'),-4),
    (('L','A'),-2),(('L','C'),-6),(('L','D'),-4),(('L','E'),-3),(('L','F'),2),(('L','G'),-4),(('L','H'),-2),(('L','I'),2),(('L','K'),-3),(('L','L'),6),(('L','M'),4),(('L','N'),-3),(('L','P'),-3),(('L','Q'),-2),(('L','R'),-3),(('L','S'),-3),(('L','T'),-2),(('L','V'),2),(('L','W'),-2),(('L','Y'),-1),
    (('M','A'),-1),(('M','C'),-5),(('M','D'),-3),(('M','E'),-2),(('M','F'),0),(('M','G'),-3),(('M','H'),-2),(('M','I'),2),(('M','K'),0),(('M','L'),4),(('M','M'),6),(('M','N'),-2),(('M','P'),-2),(('M','Q'),-1),(('M','R'),0),(('M','S'),-2),(('M','T'),-1),(('M','V'),2),(('M','W'),-4),(('M','Y'),-2),
    (('N','A'),0),(('N','C'),-4),(('N','D'),2),(('N','E'),1),(('N','F'),-3),(('N','G'),0),(('N','H'),2),(('N','I'),-2),(('N','K'),1),(('N','L'),-3),(('N','M'),-2),(('N','N'),2),(('N','P'),0),(('N','Q'),1),(('N','R'),0),(('N','S'),1),(('N','T'),0),(('N','V'),-2),(('N','W'),-4),(('N','Y'),-2),
    (('P','A'),1),(('P','C'),-3),(('P','D'),-1),(('P','E'),-1),(('P','F'),-5),(('P','G'),0),(('P','H'),0),(('P','I'),-2),(('P','K'),-1),(('P','L'),-3),(('P','M'),-2),(('P','N'),0),(('P','P'),6),(('P','Q'),0),(('P','R'),0),(('P','S'),1),(('P','T'),0),(('P','V'),-1),(('P','W'),-6),(('P','Y'),-5),
    (('Q','A'),0),(('Q','C'),-5),(('Q','D'),2),(('Q','E'),2),(('Q','F'),-5),(('Q','G'),-1),(('Q','H'),3),(('Q','I'),-2),(('Q','K'),1),(('Q','L'),-2),(('Q','M'),-1),(('Q','N'),1),(('Q','P'),0),(('Q','Q'),4),(('Q','R'),1),(('Q','S'),-1),(('Q','T'),-1),(('Q','V'),-2),(('Q','W'),-5),(('Q','Y'),-4),
    (('R','A'),-2),(('R','C'),-4),(('R','D'),-1),(('R','E'),-1),(('R','F'),-4),(('R','G'),-3),(('R','H'),2),(('R','I'),-2),(('R','K'),3),(('R','L'),-3),(('R','M'),0),(('R','N'),0),(('R','P'),0),(('R','Q'),1),(('R','R'),6),(('R','S'),0),(('R','T'),-1),(('R','V'),-2),(('R','W'),2),(('R','Y'),-4),
    (('S','A'),1),(('S','C'),0),(('S','D'),0),(('S','E'),0),(('S','F'),-3),(('S','G'),1),(('S','H'),-1),(('S','I'),-1),(('S','K'),0),(('S','L'),-3),(('S','M'),-2),(('S','N'),1),(('S','P'),1),(('S','Q'),-1),(('S','R'),0),(('S','S'),2),(('S','T'),1),(('S','V'),-1),(('S','W'),-2),(('S','Y'),-3),
    (('T','A'),1),(('T','C'),-2),(('T','D'),0),(('T','E'),0),(('T','F'),-3),(('T','G'),0),(('T','H'),-1),(('T','I'),0),(('T','K'),0),(('T','L'),-2),(('T','M'),-1),(('T','N'),0),(('T','P'),0),(('T','Q'),-1),(('T','R'),-1),(('T','S'),1),(('T','T'),3),(('T','V'),0),(('T','W'),-5),(('T','Y'),-3),
    (('V','A'),0),(('V','C'),-2),(('V','D'),-2),(('V','E'),-2),(('V','F'),-1),(('V','G'),-1),(('V','H'),-2),(('V','I'),4),(('V','K'),-2),(('V','L'),2),(('V','M'),2),(('V','N'),-2),(('V','P'),-1),(('V','Q'),-2),(('V','R'),-2),(('V','S'),-1),(('V','T'),0),(('V','V'),4),(('V','W'),-6),(('V','Y'),-2),
    (('W','A'),-6),(('W','C'),-8),(('W','D'),-7),(('W','E'),-7),(('W','F'),0),(('W','G'),-7),(('W','H'),-3),(('W','I'),-5),(('W','K'),-3),(('W','L'),-2),(('W','M'),-4),(('W','N'),-4),(('W','P'),-6),(('W','Q'),-5),(('W','R'),2),(('W','S'),-2),(('W','T'),-5),(('W','V'),-6),(('W','W'),17),(('W','Y'),0),
    (('Y','A'),-3),(('Y','C'),0),(('Y','D'),-4),(('Y','E'),-4),(('Y','F'),7),(('Y','G'),-5),(('Y','H'),0),(('Y','I'),-1),(('Y','K'),-4),(('Y','L'),-1),(('Y','M'),-2),(('Y','N'),-2),(('Y','P'),-5),(('Y','Q'),-4),(('Y','R'),-4),(('Y','S'),-3),(('Y','T'),-3),(('Y','V'),-2),(('Y','W'),0),(('Y','Y'),10)]

score :: Char -> Char -> Int
score f t = s
  where
    Just s = lookup (f,t) scores
