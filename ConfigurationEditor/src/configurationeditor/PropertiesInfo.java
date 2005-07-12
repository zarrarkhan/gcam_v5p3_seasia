package configurationeditor;

/**
 * 
 * @author Josh Lurz
 *
 * Class which contains static information about the location of the 
 * property file and specific properties.
 */
public class PropertiesInfo {
    /**
     * The name of the properties file.
     */
    public static final String PROPERTY_FILE = "configuration_editor.properties"; //$NON-NLS-1$
    
    /**
     * The name of the executable path property.
     */
    public static final String EXE_PATH_PROPERTY = "executable-path"; //$NON-NLS-1$
    
    /**
     * The name of the attribute which stores the configuration template path.
     */
    public static final String CONFIGURATION_TEMPLATE_PROPERTY = "template-path"; //$NON-NLS-1$
    
    /**
     * The name of the property which stores the location of the log configuration file.
     */
    public static final String LOG_CONF_PROPERTY = "log-conf-path"; //$NON-NLS-1$
}
