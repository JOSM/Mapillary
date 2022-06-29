// License: GPL. For details, see LICENSE file.
package org.openstreetmap.josm.plugins.mapillary.utils;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.ParserConfigurationException;

import org.apache.commons.imaging.ImageReadException;
import org.apache.commons.imaging.Imaging;
import org.openstreetmap.josm.tools.Logging;
import org.openstreetmap.josm.tools.XmlUtils;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

public final class ImageMetaDataUtil {

    private ImageMetaDataUtil() {
        // private util.
    }

    /**
     * check whether image file is a panorama photo or not.
     *
     * @param f an image file to check.
     * @return true if image is panorama photo.
     */
    public static boolean isPanorama(final File f) {
        boolean pano = false;
        try (FileInputStream fis = new FileInputStream(f)) {
            pano = isPanorama(fis);
        } catch (IOException ex) {
            Logging.trace(ex);
        }
        return pano;
    }

    /**
     * check whether image file is a panorama photo or not.
     *
     * @param is image InputStream to check.
     * @return true if image is a panorama.
     */
    public static boolean isPanorama(final InputStream is) {
        boolean pano = false;
        try {
            pano = checkXmpProjectionType(Imaging.getXmpXml(is, null), "equirectangular");
        } catch (ImageReadException | IOException ex) {
            Logging.trace(ex);
        }
        return pano;
    }

    /**
     * check XMP XML record whether projection type is equirectangle or not.
     *
     * @param xml_string XMP XML string to input.
     * @param target_type expected projection type.
     * @return true is projection type is as same as target_type.
     */
    public static boolean checkXmpProjectionType(final String xml_string, final String target_type) {
        if (xml_string == null || target_type == null) {
            return false;
        }
        boolean res = false;
        try {
            DocumentBuilder builder = XmlUtils.newSafeDOMBuilder();
            Document document = builder.parse(new InputSource(new StringReader(xml_string)));
            Element root = document.getDocumentElement();
            NodeList xmpMetaNodeList = root.getChildNodes();
            for (int i = 0; i < xmpMetaNodeList.getLength(); i++) {
                Node rdfNode = xmpMetaNodeList.item(i);
                if (rdfNode.getNodeType() == Node.ELEMENT_NODE) {
                    Element rdfElement = (Element) rdfNode;
                    NodeList rdfChildNodeList = rdfElement.getChildNodes();
                    for (int j = 0; j < rdfChildNodeList.getLength(); j++) {
                        Node rdfChildNode = rdfChildNodeList.item(j);
                        if (rdfChildNode.getNodeType() == Node.ELEMENT_NODE) {
                            Element rdfChildElement = (Element) rdfChildNode;
                            String projection = rdfChildElement
                                .getAttributeNS("http://ns.google.com/photos/1.0/panorama/", "ProjectionType");
                            if (target_type.equals(projection)) {
                                res = true;
                                break;
                            }
                        }
                    }
                }
            }
        } catch (ParserConfigurationException | SAXException | IOException ex) {
            Logging.trace(ex);
        }
        return res;
    }
}
