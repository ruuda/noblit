-- This file checks the query planner, against a music database.

-- First we set up the schema.
where
  uint64_t db.type.name "db.type.uint64"
  string_t db.type.name "db.type.string"
  ref_t db.type.name "db.type.ref"

assert
  artist_name db.attribute.name "artist.name"
  artist_name db.attribute.type string_t
  artist_name db.attribute.unique false
  artist_name db.attribute.many false

  album_title db.attribute.name "album.title"
  album_title db.attribute.type string_t
  album_title db.attribute.unique false
  album_title db.attribute.many false

  album_artist db.attribute.name "album.artist"
  album_artist db.attribute.type ref_t
  album_artist db.attribute.unique false
  album_artist db.attribute.many false

  album_release_year db.attribute.name "album.release_year"
  album_release_year db.attribute.type uint64_t
  album_release_year db.attribute.unique false
  album_release_year db.attribute.many false

  album_release_month db.attribute.name "album.release_month"
  album_release_month db.attribute.type uint64_t
  album_release_month db.attribute.unique false
  album_release_month db.attribute.many false

  album_release_day db.attribute.name "album.release_day"
  album_release_day db.attribute.type uint64_t
  album_release_day db.attribute.unique false
  album_release_day db.attribute.many false

  track_title db.attribute.name "track.title"
  track_title db.attribute.type string_t
  track_title db.attribute.unique false
  track_title db.attribute.many false

  track_artist db.attribute.name "track.artist"
  track_artist db.attribute.type ref_t
  track_artist db.attribute.unique false
  track_artist db.attribute.many false

  track_album db.attribute.name "track.album"
  track_album db.attribute.type ref_t
  track_album db.attribute.unique false
  track_album db.attribute.many false

  track_disc db.attribute.name "track.disc"
  track_disc db.attribute.type uint64_t
  track_disc db.attribute.unique false
  track_disc db.attribute.many false

  track_number db.attribute.name "track.number"
  track_number db.attribute.type uint64_t
  track_number db.attribute.unique false
  track_number db.attribute.many false

  track_duration_seconds db.attribute.name "track.duration_seconds"
  track_duration_seconds db.attribute.type uint64_t
  track_duration_seconds db.attribute.unique false
  track_duration_seconds db.attribute.many false

-- We selected nothing, so the output is empty.

> ┌──┐
> │  │
> ├──┤
> │  │
> └──┘

-- Then we can check some query plans.

-- Select all tracks titled "Time".
explain
where
  tr track.title "Time"
  tr track.artist ta
  tr track.album ab
  ab album.title album_title
  ab album.release_year year
  ta artist.name track_artist
select
  year, track_artist, album_title

> let :6 = Value(9511602414708156756)
> for tr:0 in avet (value=:6)
> for ta:1 in aevt (entity=tr:0)
> for ab:2 in aevt (entity=tr:0)
> for album_title:3 in aevt (entity=ab:2)
> for year:4 in aevt (entity=ab:2)
> for track_artist:5 in aevt (entity=ta:1)
> yield year:4, track_artist:5, album_title:3

-- Select all tracks by Daft Punk.
explain
where
  aa artist.name "Daft Punk"
  ab album.artist aa
  ab album.title album_title
  tr track.album ab
  tr track.disc disc
  tr track.number number
  tr track.title track_title
  tr track.artist ta
  ta artist.name track_artist

select
  disc, number, track_title, track_artist, album_title

> let :9 = Value(13835058055282163713)
> for aa:0 in avet (value=:9)
> for ab:1 in avet (value=aa:0)
> for album_title:2 in aevt (entity=ab:1)
> for tr:3 in avet (value=ab:1)
> for disc:4 in aevt (entity=tr:3)
> for number:5 in aevt (entity=tr:3)
> for track_title:6 in aevt (entity=tr:3)
> for ta:7 in aevt (entity=tr:3)
> for track_artist:8 in aevt (entity=ta:7)
> yield disc:4, number:5, track_title:6, track_artist:8, album_title:2

-- Select all artists that have released a track called "Time",
-- as well as a track called "Money".
explain
where
  t_time track.title "Time"
  t_money track.title "Money"
  t_time track.album a_time
  t_money track.album a_money
  a_time album.artist artist
  a_money album.artist artist
select
  artist

> let :5 = Value(9511602414708156756)
> let :6 = Value(9583660528437194573)
> for t_time:0 in avet (value=:5)
> for t_money:1 in avet (value=:6)
> for a_time:2 in aevt (entity=t_time:0)
> for a_money:3 in aevt (entity=t_money:1)
> for artist:4 in aevt (entity=a_time:2)
> for _ in aevt (entity=a_money:3, value=artist:4)
> yield artist:4

-- Same as above, but in reverse.
explain
where
  a_time album.artist artist
  a_money album.artist artist
  t_time track.album a_time
  t_money track.album a_money
  t_time track.title "Time"
  t_money track.title "Money"
select
  artist

> let :5 = Value(9511602414708156756)
> let :6 = Value(9583660528437194573)
> for a_time:1, artist:0 in aevt
> for a_money:2 in avet (value=artist:0)
> for t_time:3 in avet (value=a_time:1)
> for t_money:4 in avet (value=a_money:2)
> for _ in aevt (entity=t_time:3, value=:5)
> for _ in aevt (entity=t_money:4, value=:6)
> yield artist:0
