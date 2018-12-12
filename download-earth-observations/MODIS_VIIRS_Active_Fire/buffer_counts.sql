alter table "RA2018".station_locations_dates add column buffer_25k_geog geography;
alter table "RA2018".station_locations_dates add column buffer_500k_geog geography;
alter table "RA2018".station_locations_dates add column buffer_2000k_geog geography;
update "RA2018".station_locations_dates set buffer_25k_geog geography = st_buffer(geom::geography, 25000);
update "RA2018".station_locations_dates set buffer_500k_geog geography = st_buffer(geom::geography, 500000);
update "RA2018".station_locations_dates set buffer_2000k_geog geography = st_buffer(geom::geography, 2000000);



############################
alter table "RA2018".station_locations_dates add column buffer_25k_count integer;
alter table "RA2018".station_locations_dates add column buffer_500k_count integer;
alter table "RA2018".station_locations_dates add column buffer_2000k_count integer;

CREATE INDEX date_idx ON "RA2018".station_locations_dates (date);
CREATE INDEX adj_date_idx ON "RA2018".fires_modis (adj_date);

update "RA2018".station_locations_dates set buffer_25k_count =
(select count(t2.adj_date) from "RA2018".fires_modis t2 where st_intersects(t1.buffer_25k_geog, t2.geom::geography)
and t1.date=t2.adj_date::date)

update "RA2018".station_locations_dates set buffer_500k_count =
(select count(*) from "RA2018".fires_modis t1 join "RA2018".station_locations_dates t2 on 
st_intersects(t2.buffer_500k_geog, t1.geom::geography) where t1.adj_date::date=t2.date);

update "RA2018".station_locations_dates set buffer_2000k_count =
(select count(*) from "RA2018".fires_modis t1 join "RA2018".station_locations_dates t2 on 
st_intersects(t2.buffer_2000k_geog, t1.geom::geography) where t1.adj_date::date=t2.date);


