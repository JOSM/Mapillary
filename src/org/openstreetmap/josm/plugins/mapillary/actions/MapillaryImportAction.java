package org.openstreetmap.josm.plugins.mapillary.actions;

import static org.openstreetmap.josm.tools.I18n.tr;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.IOException;

import javax.swing.JFileChooser;
import javax.swing.filechooser.FileNameExtensionFilter;

import org.apache.commons.imaging.ImageReadException;
import org.apache.commons.imaging.Imaging;
import org.apache.commons.imaging.common.ImageMetadata;
import org.apache.commons.imaging.common.RationalNumber;
import org.apache.commons.imaging.formats.jpeg.JpegImageMetadata;
import org.apache.commons.imaging.formats.tiff.TiffField;
import org.apache.commons.imaging.formats.tiff.constants.GpsTagConstants;
import org.openstreetmap.josm.Main;
import org.openstreetmap.josm.actions.JosmAction;
import org.openstreetmap.josm.data.coor.LatLon;
import org.openstreetmap.josm.plugins.mapillary.MapillaryData;
import org.openstreetmap.josm.plugins.mapillary.MapillaryImportedImage;
import org.openstreetmap.josm.plugins.mapillary.MapillaryLayer;
import org.openstreetmap.josm.tools.ImageProvider;
import org.openstreetmap.josm.tools.Shortcut;

public class MapillaryImportAction extends JosmAction {

	public JFileChooser chooser;

	private int noTagsPics = 0;

	public MapillaryImportAction() {
		super(tr("Import pictures"), new ImageProvider("icon24.png"),
				tr("Import local pictures"), Shortcut.registerShortcut(
						"Import Mapillary",
						tr("Import pictures into Mapillary layer"),
						KeyEvent.VK_M, Shortcut.NONE), false,
				"mapillaryImport", false);
	}

	@Override
	public void actionPerformed(ActionEvent e) {
		chooser = new JFileChooser();
		chooser.setCurrentDirectory(new java.io.File(System
				.getProperty("user.home")));
		chooser.setDialogTitle(tr("Select pictures"));
		chooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
		chooser.setAcceptAllFileFilterUsed(false);
		chooser.addChoosableFileFilter(new FileNameExtensionFilter("images",
				"jpg", "jpeg", "png"));
		chooser.setMultiSelectionEnabled(true);
		if (chooser.showOpenDialog(Main.parent) == JFileChooser.APPROVE_OPTION) {
			for (int i = 0; i < chooser.getSelectedFiles().length; i++) {
				File file = chooser.getSelectedFiles()[i];
				if (file.isDirectory()) {

				} else {
					if (file.getPath().substring(file.getPath().length() - 4)
							.equals(".jpg")
							|| file.getPath()
									.substring(file.getPath().length() - 5)
									.equals(".jpeg")) {
						try {
							readJPG(file);
						} catch (ImageReadException ex) {
							Main.error(ex);
						} catch (IOException ex) {
							Main.error(ex);
						}
					} else if (file.getPath()
							.substring(file.getPath().length() - 4)
							.equals(".png")) {
						readPNG(file);
					}
				}
			}
		}
		MapillaryLayer.getInstance();
	}

	public void readJPG(File file) throws ImageReadException, IOException {
		final ImageMetadata metadata = Imaging.getMetadata(file);
		if (metadata instanceof JpegImageMetadata) {
			final JpegImageMetadata jpegMetadata = (JpegImageMetadata) metadata;
			final TiffField lat_ref = jpegMetadata
					.findEXIFValueWithExactMatch(GpsTagConstants.GPS_TAG_GPS_LATITUDE_REF);
			final TiffField lat = jpegMetadata
					.findEXIFValueWithExactMatch(GpsTagConstants.GPS_TAG_GPS_LATITUDE);
			final TiffField lon_ref = jpegMetadata
					.findEXIFValueWithExactMatch(GpsTagConstants.GPS_TAG_GPS_LONGITUDE_REF);
			final TiffField lon = jpegMetadata
					.findEXIFValueWithExactMatch(GpsTagConstants.GPS_TAG_GPS_LONGITUDE);
			final TiffField ca = jpegMetadata
					.findEXIFValueWithExactMatch(GpsTagConstants.GPS_TAG_GPS_IMG_DIRECTION);
			double latValue = 0;
			double lonValue = 0;
			double caValue = 0;
			if (lat != null && lat.getValue() instanceof RationalNumber[])
				latValue = DegMinSecToDouble((RationalNumber[]) lat.getValue(),
						lat_ref.getValue().toString());
			if (lon != null && lon.getValue() instanceof RationalNumber[])
				lonValue = DegMinSecToDouble((RationalNumber[]) lon.getValue(),
						lon_ref.getValue().toString());
			if (ca != null && ca.getValue() instanceof RationalNumber)
				caValue = ((RationalNumber) ca.getValue()).doubleValue();
			if (lat_ref.getValue().toString().equals("S"))
				latValue = -latValue;
			if (lon_ref.getValue().toString().equals("W"))
				lonValue = -lonValue;
			if (latValue != 0 && lonValue != 0) {
				MapillaryData.getInstance().add(
						new MapillaryImportedImage(latValue, lonValue, caValue,
								file));
			} else {
				readNoTags(file);
			}
		}
	}

	private void readNoTags(File file) {
		double HORIZONTAL_DISTANCE = 0.0001;
		double horDev;
		if (noTagsPics % 2 == 0)
			horDev = HORIZONTAL_DISTANCE * noTagsPics / 2;
		else
			horDev = -HORIZONTAL_DISTANCE * (noTagsPics + 1) / 2;
		LatLon pos = Main.map.mapView.getProjection().eastNorth2latlon(
				Main.map.mapView.getCenter());
		MapillaryData.getInstance().add(
				new MapillaryImportedImage(pos.lat(), pos.lon() + horDev, 0, file));
		noTagsPics++;
	}

	private void readPNG(File file) {
		readNoTags(file);
	}

	private double DegMinSecToDouble(RationalNumber[] degMinSec, String ref) {
		RationalNumber deg = degMinSec[0];
		RationalNumber min = degMinSec[1];
		RationalNumber sec = degMinSec[2];
		return deg.doubleValue() + min.doubleValue() / 60 + sec.doubleValue()
				/ 3600;
	}
}
