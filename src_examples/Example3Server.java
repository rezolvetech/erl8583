import org.jpos.iso.*;
import org.jpos.iso.channel.ASCIIChannel;
import org.jpos.iso.packager.ISO87APackager;

public class Example3Server implements ISORequestListener {

	public static void main(String[] args) throws Exception {
		ServerChannel channel = new ASCIIChannel(new ISO87APackager());
		ISOServer server = new ISOServer(8000, channel, null);
		server.addISORequestListener(new Example3Server());
		new Thread(server).start();
	}

	@Override
	public boolean process(ISOSource source, ISOMsg m) {
		try {
			m.setResponseMTI();
			source.send(m);
			return true;
		} catch (Exception e) {
			e.printStackTrace();
			return false;
		}
	}

}
