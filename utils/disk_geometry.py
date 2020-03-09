def chs(lba,C=1024,H=255,S=63):
  """
  'lba' linearly addresses sector, indexing from zero.
  'C','H','S' specify geometry - fixed for a given disk:
     1 <= C <= 1024 (10 bits)
     1 <= H <= 255  (8 bits) not 256 due to WD1010 quirk
     1 <= S <= 63   (6 Bits) not 64 due to WD1010 quirk
   Returns address as c,h,s tuple:
     0 <= c <= 1023 (10 bits) modulo C
     0 <= h <= 255  (8 bits) modulo H
     1 <= s <= 63   (6 Bits) not 64 due to WD1010 quirk
  """
  if C<1 or H<1 or S<1 or C>1024 or H>255 or S>63:
    raise ValueError
  t,s = divmod(lba,S); s+=1 # tracks, sector offset
  c,h = divmod(t,H)
  if c>=C: raise ValueError
  return (c,h,s)


if __name__ == "__main__":
    print(chs(2879, C=80,H=2,S=18))
