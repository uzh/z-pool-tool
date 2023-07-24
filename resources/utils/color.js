import crypto from 'crypto';

export const generateColor = (str, opacity = 1) => `#${crypto.createHash('md5')
    .update(str)
    .digest('hex')
    .slice(0, 6)}${Math.floor(opacity * 255).toString(16)}`;
